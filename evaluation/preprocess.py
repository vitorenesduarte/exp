#!/usr/bin/env python

import os, os.path, json
import shutil

METRIC_DIR = "metrics"
PROCESSED_DIR = "processed"
CONFIG_FILE = "rsg.json"
TS="ts"
SIZE="size"
COMPRESS=12 # every x
#MAX_TIME=60

def error(message):
    """
    Display error message and exit.
    """
    print(message)
    print("Exiting...")
    exit()

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
    return [os.path.join(dir, f) for f in os.listdir(dir) if filter(f) and f[0] != '.']

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
        "exp_gmap_simulation_key_percentage",
        "exp_simulation",
        "exp_overlay",
        "exp_node_number",
        "ldb_mode",
        "ldb_redundant_dgroups",
        "ldb_dgroup_back_propagation"
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
        if k in r:
            error("key " + k + " already found!")

        r[k] = {}

        for file in d[dir]:
            # read metric file
            json = read_json(file)

            # for all time-series types (all but processing)
            # for all metrics remove start_time
            for type in json:
                if not type in ["processing", "latency"]:
                    for m in json[type]:
                        m[TS] -= start_time

                # store metrics by type
                r[k][type] = json[type]

    return r

def get_higher_ts(run):
    """
    Find the higher timestamp of run.
    """
    higher = 0
    for metric in run:
        higher = max(higher, metric[TS])

    return higher

def bottom_size(type):
    """
    Return bottom size.
    """

    if type in ["transmission", "memory"]:
        return [0, 0]

    error("type not found.")

def add(type, sizeA, sizeB):
    """
    Sum two sizes
    """

    if type in ["transmission", "memory"]:
        return [sizeA[0] + sizeB[0], sizeA[1] + sizeB[1]]

    error("type not found")

def default(type, previous):
    """
    Default value given a type:
    - if transmission, 0
    - if memory, previous value
    """
    one = ["transmission"]
    two = ["memory"]

    if type in one:
        return [0, 0]
    if type in two:
        return previous

    error("type not found")

def ignore_pre_big_bang(run):
    """
    Remove metrics before timestamp 0.
    """
    
    return [m for m in run if m[TS] >= 0]
    #return [m for m in run if m[TS] >= 0 and m[TS] < MAX_TIME]

def assume_unknown_values(d):
    """
    Assume values for timestamps not reported for transmission graphs.
    """

    for key in d:

        # get all time-series types
        types = ["transmission", "memory"]

        for type in types:
            run = d[key][type]

            # find the higher timestamp
            higher_ts = get_higher_ts(run)

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
            d[key][type] = ts_to_size

    return d

def sum_lists(ls):
    """
    Sum two lists.
    """
    return [sum(x) for x in zip(*ls)]

def divide_lists(ls, by):
    """
    Divide lists by 'by'.
    """
    return [x / float(by) for x in ls]

def to_ms(microseconds):
    """
    Convertes microseconds to milliseconds.
    """
    return microseconds / float(1000)

def aggregate(d):
    """
    Aggregate types of the same run.
    """

    def get_compress_index(key):
        m = {
            110: 1,
            210: 5,
            310: 2,
            420: 4,
            430: 6,
            440: 3,
            450: 0
        }

        score = get_score(key)
        if score in m:
            return (COMPRESS * m[score]) / len(m)
        else:
            return 0

    r = {}

    for key in d:
        # create key in dictionary
        r[key] = {}
        r[key]["transmission_metadata"] = []
        r[key]["transmission_crdt"] = []
        r[key]["transmission"] = []
        r[key]["memory_algorithm"] = []
        r[key]["memory_crdt"] = []
        r[key]["processing"] = d[key]["processing"]

        # group transmission
        for [M, C] in d[key]["transmission"].itervalues():
            r[key]["transmission_metadata"].append(M)
            r[key]["transmission_crdt"].append(C)
            r[key]["transmission"].append(M + C)

        # compress transmissions
        # e.g. sum every 10 values
        # and average them
        xs = []
        ys = []
        current_sum = 0
        run_len = len(r[key]["transmission"])
        index = get_compress_index(key)

        for i in range(run_len):
            # update sum
            current_sum += r[key]["transmission"][i]

            if(i % COMPRESS == index):
                ys.append(current_sum)
                # reset sum
                current_sum = 0

        for i in range(len(ys)):
            xs.append((i * COMPRESS) + index)

        ys = divide_lists(ys, COMPRESS)
        r[key]["transmission_compressed"] = ys
        r[key]["transmission_compressed_x"] = xs

        # aggregate memory
        for [A, C] in d[key]["memory"].itervalues():
            r[key]["memory_algorithm"].append(A)
            r[key]["memory_crdt"].append(C)
        
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

def get_score(type):
    """
    Returns the order of this type when drawing.
    """
    score = 0

    parts = type.split("~")
    mode = parts[4]
    delta_mode = "_".join(parts[5:])

    if mode == "state_based":
        score += 100
    elif mode == "vanilla_scuttlebutt":
        score += 200
    elif mode == "scuttlebutt":
        score += 300
    elif mode == "delta_based":
        score += 400
    else:
        error("Mode not found")

    if delta_mode == "undefined_undefined":
        score += 10
    elif delta_mode == "False_False":
        score += 20
    elif delta_mode == "False_True":
        score += 30
    elif delta_mode == "True_False":
        score += 40
    elif delta_mode == "True_True":
        score += 50
    else:
        error("Delta mode not found")
        
    return score

def dump(d):
    """
    Save average to files.
    """

    # clear folder
    shutil.rmtree(PROCESSED_DIR, ignore_errors=True)

    for type in d:
        score = get_score(type)
        path = os.path.join(*[PROCESSED_DIR, str(score) + "~" + type])
        content = json.dumps(d[type])
        save_file(path, content)

def main():
    """
    Main.
    """
    d = get_metric_files()
    d = group_by_config(d)
    d = assume_unknown_values(d)
    d = aggregate(d)
    dump(d)

main()
