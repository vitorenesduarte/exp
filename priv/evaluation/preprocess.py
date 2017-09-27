#!/usr/bin/env python

import os, os.path, json
import shutil

METRIC_DIR = "metrics"
PROCESSED_DIR = "processed"
CONFIG_FILE = "rsg.json"
TS="ts"
SIZE="size"

def ls(dir):
    """
    List a directory, returning full path.
    """
    return ls_grep(dir, lambda x: True)

def ls_grep(dir, filter):
    """
    List a directory, returning full path.
    Only files that match the filter are returned.
    """
    return [os.path.join(dir, f) for f in os.listdir(dir) if filter(f)]

def get_metric_files():
    """
    Return a dictionary from run (unique timestamp)
    to list of metric files.
    """
    d = {}
    dirs = ls(METRIC_DIR)

    for dir in dirs:
        # only list files that are not the config file
        files = ls_grep(dir, lambda x: x != CONFIG_FILE)
        d[dir] = files

    return d

def read_json(f):
    """
    Read a json file.
    """
    r = {}
    with open(f) as fd:
        r = json.load(fd)

    return r

def key(config):
    """
    Create a key from a config file.
    """

    # get start_time from config
    start_time = config["start_time"]

    keys = [
        "lsim_simulation",
        "lsim_overlay",
        "lsim_node_number",
        "lsim_node_event_number",
        "lsim_element_node_ratio",
        "lsim_partition_number"
    ]

    l = []
    for k in keys:
        l.append(str(config[k]))

    k = "~".join(l)
    return (start_time, k)

def group_by_config(d):
    """
    Given metric files, group them by config file.
    """
    r = {}
    
    for dir in d:
        config_path = os.path.join(dir, CONFIG_FILE)
        (start_time, k) = key(
            read_json(config_path)
        )
        
        # create empty dictionary if key not already in dictionary
        if not k in r:
            r[k] = {}

        for file in d[dir]:
            # read metric file
            j = read_json(file)

            # for all time-series types (all but latency)
            # for all metrics remove start_time

            for type in j:

                if type != "latency":
                    for m in j[type]:
                        m[TS] -= start_time

                # create empty list if type not already in dictionary
                if not type in r[k]:
                    r[k][type] = []

                # store metrics by type
                r[k][type].append(j[type])

    return r

def get_higher_ts(runs):
    """
    Find the higher timestamp of all runs.
    """
    higher = 0
    for run in runs:
        for metric in run:
            higher = max(higher, metric[TS])

    return higher

def bottom_size(type):
    """
    Return bottom size depending on the type passed as input.
    """
    one = ["tcbcast", "tcbcast_ack"]
    two = ["memory"]

    if type in one:
        return [0]
    if type in two:
        return [0, 0]

    print("type not found. Exiting.")
    exit()

def add(type, sizeA, sizeB):
    """
    Sum two sizes
    """

    one = ["tcbcast", "tcbcast_ack"]
    two = ["memory"]

    if type in one:
        return [sizeA[0] + sizeB[0]]
    if type in two:
        return [sizeA[0] + sizeB[0], sizeA[1] + sizeB[1]]

    print("type not found. Exiting.")
    exit()

def default(type, previous):
    """
    Default value given a type:
    - if transmission, 0
    - if memory, previous value
    """
    one = ["tcbcast", "tcbcast_ack"]
    two = ["memory"]

    if type in one:
        return [0]
    if type in two:
        return previous

    print("type not found. Exiting.")
    exit()

def ignore_pre_big_bang(run):
    """
    Remove metrics before timestamp 0.
    """
    
    return [m for m in run if m[TS] >= 0]


def assume_unknown_values(d):
    """
    Assume values for timestamps not reported for transmission graphs.
    """

    for key in d:

        # get all time-series types
        types = d[key].keys()
        types.remove("latency")

        for type in types:

            # find the higher timestamp of all runs for this type
            higher_ts = get_higher_ts(d[key][type])

            runs = []

            for run in d[key][type]:

                # remove timestamps before 0
                run = ignore_pre_big_bang(run)

                # get bottom size
                bs = bottom_size(type)

                # since we can have several metrics
                # for the same timestamp,
                # aggregate metrics per timestamp
                ts_to_size = {}

                for metric in run:
                    ts = metric[TS]
                    size = metric[SIZE]

                    # if ts not in map
                    # create an entry
                    if not ts in ts_to_size:
                        ts_to_size[ts] = bs

                    ts_to_size[ts] = add(type, ts_to_size[ts], size)

                previous = bs

                # create bottom values for unknown timestamps
                for ts in range(0, higher_ts):
                    if not ts in ts_to_size:
                        ts_to_size[ts] = default(type, previous)

                    previous = ts_to_size[ts]

                # store the ts_to_size map
                runs.append(ts_to_size)

            # update all runs
            d[key][type] = runs

    return d

def sum_lists(ls):
    """
    Sum two lists.
    """
    return [sum(x) for x in zip(*ls)]

def divide_list_by(ls, n):
    """
    Divide all elements of list by n.
    """
    return [e / n for e in ls]

def average(d):
    """
    Average runs.
    """

    for key in d:

        # get all time-series types
        types = d[key].keys()
        types.remove("latency")

        for type in types:
            # number of runs
            runs_number = len(d[key][type])
            # number of metrics
            metrics_number = len(d[key][type][0]) - 1

            # get bottom size
            bs = bottom_size(type)

            # list where we'll store the sum of the sizes
            sum = [bs for i in range(0, metrics_number)]

            # sum all runs
            for run in d[key][type]:
                for i in range(0, metrics_number):
                    ls = [
                        sum[i],
                        run[i]
                    ]
                    sum[i] = sum_lists(ls)

            # avg of sum
            avg = [divide_list_by(ls, runs_number) for ls in sum]

            # store avg
            d[key][type] = avg

    return d

def to_ms(microseconds):
    """
    Convertes microseconds to milliseconds.
    """
    return microseconds / float(1000)

def aggregate(d):
    """
    Aggregate types of the same run.
    """

    r = {}

    for key in d:
        # create key in dictionary
        r[key] = {}

        # sum all lists that have these types
        to_sum = []
        for type in d[key]:
            if type in ["tcbcast", "tcbcast_ack"]:
                # make list of lists into list
                ls = [e for l in d[key][type] for e in l]
                to_sum.append(ls)

        r[key]["transmission"] = sum_lists(to_sum)
        r[key]["memory_crdt"] = []
        r[key]["memory_algorithm"] = []
        r[key]["latency_local"] = []
        r[key]["latency_remote"] = []

        # aggregate memory values
        for [C, R] in d[key]["memory"]:
            r[key]["memory_crdt"].append(C)
            r[key]["memory_algorithm"].append(R)

        
        # aggregate latency values
        for lord in d[key]["latency"]: # local or remote dict
            for lort in lord: # local or remote type
                k = "latency_" + lort
                latency_values = map(to_ms, lord[lort])
                r[key][k].extend(latency_values)

    return r

def group_by_simulation(d):
    """
    Group metrics by simulation (gset, awset, ...).
    """

    r = {}

    for type in d:
        simulation = type.split("~")[0]

        if not simulation in r:
            r[simulation] = {}

        r[simulation][type] = d[type]

    return r

def save_file(path, content):
    """
    Save content in path.
    """

    dir = os.path.dirname(path)

    # ensure directory exist
    if not os.path.exists(dir):
        os.makedirs(dir)

    # write content
    with open(path, "w") as fd:
        fd.write(content)

def dump(d):
    """
    Save average to files.
    """

    # clear folder
    shutil.rmtree(PROCESSED_DIR, ignore_errors=True)

    for simulation in d:
        for type in d[simulation]:
            path = os.path.join(*[PROCESSED_DIR, simulation, type])
            content = json.dumps(d[simulation][type])
            save_file(path, content)

def main():
    """
    Main.
    """
    d = get_metric_files()
    d = group_by_config(d)
    d = assume_unknown_values(d)
    d = average(d)
    d = aggregate(d)
    d = group_by_simulation(d)
    dump(d)

main()
