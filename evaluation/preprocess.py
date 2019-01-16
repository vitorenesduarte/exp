#!/usr/bin/env python

import os, os.path, json
import shutil

METRIC_DIR = "metrics"
PROCESSED_DIR = "processed"
CONFIG_FILE = "rsg.json"
TS="ts"
SIZE="size"
TERM_SIZE="term_size"
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
        "exp_retwis_zipf",
        "exp_gmap_simulation_key_percentage",
        "exp_simulation",
        "exp_overlay",
        "exp_node_number",
        "ldb_mode",
        "ldb_redundant_dgroups",
        "ldb_dgroup_back_propagation",
        "ldb_scuttlebutt_gc",
        "ldb_op_ii"
    ]

    l = []
    for k in keys:
        v = "undefined"
        if k in config:
            v = str(config[k])
        l.append(v)

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
                if type in ["transmission", "memory"]:
                    # init type
                    if not type in r[k]:
                        r[k][type] = []

                    # store it
                    for m in json[type]:
                        m[TS] -= start_time
                    r[k][type].append(json[type])

                elif type == "processing":
                    # init type
                    if not type in r[k]:
                        r[k][type] = 0

                    # store it
                    r[k][type] += json[type]

                elif type == "latency":
                    # init type
                    if not type in r[k]:
                        r[k][type] = {}

                    for latency_type in json[type]:
                        # init latency type
                        if not latency_type in r[k][type]:
                            r[k][type][latency_type] = []

                        # store it
                        r[k][type][latency_type].extend(json[type][latency_type])

    return r

def get_higher_ts(runs):
    """
    Find the higher timestamp of run.
    """
    higher = 0
    for run in runs:
        for metric in run:
            higher = max(higher, metric[TS])

    return higher

def bottom_size(type):
    """
    Return bottom size.
    """

    if type in ["transmission", "memory"]:
        return [0, 0, 0]

    error("type not found.")

def add(type, sizeA, sizeB):
    """
    Sum two sizes
    """

    if type in ["transmission", "memory"]:
        return [sizeA[0] + sizeB[0],
                sizeA[1] + sizeB[1],
                sizeA[2] + sizeB[2]]

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
        return [0, 0, 0]
    if type in two:
        return previous

    error("type not found")

def ignore_pre_big_bang(run):
    """
    Remove metrics before timestamp 0.
    """
    
    return [m for m in run if m[TS] >= 0]
    #return [m for m in run if m[TS] >= 0 and m[TS] < MAX_TIME]

def b_to_mb(bytes):
    """
    Convert from bytes to gigabytes.
    """
    return bytes / 1000

def assume_unknown_values(d):
    """
    Assume values for timestamps not reported for transmission graphs.
    """
    
    for key in d:
        # get all time-series types
        types = ["transmission", "memory"]

        for type in types:
            runs = []
            # find the higher timestamp for all runs
            higher_ts = get_higher_ts(d[key][type])

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
                    term_size = b_to_mb(metric[TERM_SIZE])
                    size.append(term_size)

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

            d[key][type] = runs

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

def average(d):
    """
    Average runs.
    """
    for key in d:
        # get all time-series types
        types = ["transmission", "memory"]
        
        for type in types:
            # number of runs
            runs_number = len(d[key][type])
            # number of metrics (all but processing)
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
            avg = [divide_lists(ls, runs_number) for ls in sum]
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

    def get_compress_index(key):
        m = {
            110: 1,
            220: 5,
            230: 8,
            340: 2,
            350: 4,
            460: 6,
            470: 3,
            480: 0,
            490: 7
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
        r[key]["transmission_term_size"] = []
        r[key]["memory_algorithm"] = []
        r[key]["memory_crdt"] = []
        r[key]["memory_term_size"] = []
        r[key]["processing"] = d[key]["processing"]
        r[key]["latency"] = d[key]["latency"]

        # group transmission
        for [M, C, T] in d[key]["transmission"]:
            r[key]["transmission_metadata"].append(M)
            r[key]["transmission_crdt"].append(C)
            r[key]["transmission"].append(M + C)
            r[key]["transmission_term_size"].append(T)

        # compress transmissions (total and crdt only)
        # e.g. sum every 10 values 
        # and average them
        for to_compress_key in ["transmission", "transmission_crdt"]:
            xs = []
            ys = []
            current_sum = 0
            run_len = len(r[key][to_compress_key])
            index = get_compress_index(key)

            for i in range(run_len):
                # update sum
                current_sum += r[key][to_compress_key][i]

                if(i % COMPRESS == index):
                    ys.append(current_sum)
                    # reset sum
                    current_sum = 0

            for i in range(len(ys)):
                xs.append((i * COMPRESS) + index)

            ys = divide_lists(ys, COMPRESS)
            r[key][to_compress_key + "_compressed"] = ys
            r[key][to_compress_key + "_compressed_x"] = xs

        # aggregate memory
        for [A, C, T] in d[key]["memory"]:
            r[key]["memory_algorithm"].append(A)
            r[key]["memory_crdt"].append(C)
            r[key]["memory_term_size"].append(T)
        
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
    mode = parts[5]
    rest = "_".join(parts[6:])

    if mode == "state_based":
        score += 100
    elif mode == "op_based":
        score += 200
    elif mode == "scuttlebutt":
        score += 300
    elif mode == "delta_based":
        score += 400
    else:
        error("Mode not found: " + mode)

    if rest == "undefined_undefined_undefined_undefined":
        score += 10
    elif rest == "undefined_undefined_undefined_False":
        score += 20
    elif rest == "undefined_undefined_undefined_True":
        score += 30
    elif rest == "undefined_undefined_False_undefined":
        score += 40
    elif rest == "undefined_undefined_True_undefined":
        score += 50
    elif rest == "False_False_undefined_undefined":
        score += 60
    elif rest == "False_True_undefined_undefined":
        score += 70
    elif rest == "True_False_undefined_undefined":
        score += 80
    elif rest == "True_True_undefined_undefined":
        score += 90
    else:
        error("Remaining configuration not found: " + rest)

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
    d = average(d)
    d = aggregate(d)
    dump(d)

main()
