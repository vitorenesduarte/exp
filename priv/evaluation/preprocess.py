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
        "ldb_mode",
        "ldb_driven_mode",
        "ldb_state_sync_interval",
        "ldb_redundant_dgroups",
        "ldb_dgroup_back_propagation"
    ]

    l = []
    for k in keys:
        l.append(str(config[k]))

    k = "-".join(l)
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

def get_first_ts(run):
    """
    Find the first timestamp of a run.
    """
    return run[0][TS]

def create_metric(ts, size):
    """
    Create metric from timestamp and size.
    """
    metric = {}
    metric[TS] = ts
    metric[SIZE] = size
    return metric

def bottom_size(type):
    """
    Return bottom size depending on the type passed as input.
    """
    one = ["state", "digest", "delta", "delta_ack"]
    two = ["memory"]

    if type in one:
        return [0]
    if type in two:
        return [0, 0]

    print("type not found. Exiting.")
    exit()

def ignore_pre_big_bang(run):
    """
    Remove metrics before timestamp 0.
    """
    
    return [m for m in run if m[TS] >= 0]


def assume_unknown_values(d):
    """
    Assume values for timestamps not reported.
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

                # find the first timestamp of this run
                first_ts = get_first_ts(run)

                # get bottom size
                bs = bottom_size(type)

                # create fake values from timestamp 0 to first_ts
                for i in range(0, first_ts):
                    metric = create_metric(i, bs)
                    run.insert(i, metric)

                # create fake values for unkown timestamps
                last_size = bs
                for i in range(0, higher_ts + 1):
                    if i >= len(run) or run[i][TS] != i:
                        # if timestamp not found
                        # create metric with last known value
                        metric = create_metric(i, last_size)
                        run.insert(i, metric)
                    else:
                        # else update last known value
                        last_size = run[i][SIZE]

                # store the updated run
                runs.append(run)

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
            metrics_number = len(d[key][type][0])

            # get bottom size
            bs = bottom_size(type)

            # list where we'll store the sum of the sizes
            sum = [bs for i in range(0, metrics_number)]

            # sum all runs
            for run in d[key][type]:
                for i in range(0, metrics_number):
                    ls = [
                        sum[i],
                        run[i][SIZE]
                    ]
                    sum[i] = sum_lists(ls)

            # avg of sum
            avg = [divide_list_by(ls, runs_number) for ls in sum]

            # store avg
            d[key][type] = avg

    return d

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
            if type in ["state", "digest", "delta", "delta_ack"]:
                # make list of lists into list
                ls = [e for l in d[key][type] for e in l]
                to_sum.append(ls)

        r[key]["transmission"] = sum_lists(to_sum)
        r[key]["crdt"] = []
        r[key]["rest"] = []

        for [C, R] in d[key]["memory"]:
            r[key]["crdt"].append(C)
            r[key]["rest"].append(R)

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

    for key in d:
        for file in d[key]:
            avg = d[key][file]
            path = os.path.join(*[PROCESSED_DIR, key, file])
            content = json.dumps(avg)
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
    print(d)
    dump(d)

main()
