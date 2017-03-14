#!/usr/bin/env python

import os, os.path, json

METRIC_DIR = "metrics"
CONFIG_FILE = "rsg.json"

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

    # remove start time from config
    start_time = config.pop("start_time")

    l = []
    for k in config:
        l.append(k)
        l.append(str(config[k]))

    k = "-".join(l)
    return (start_time, k)

def group_by_config(d):
    """
    Given metric files, group them by config file.
    """
    r = {}
    key_to_config = {}
    
    for dir in d:
        config_path = os.path.join(dir, CONFIG_FILE)
        (start_time, k) = key(
            read_json(config_path)
        )
        
        # store config file
        key_to_config[k] = config_path

        # create empty dictionary if key not already in dictionary
        if not k in r:
            r[k] = {}

        for file in d[dir]:
            # read metric file
            j = read_json(file)
            # for all types, for all metrics
            # remove start_time
            for type in j:
                for m in j[type]:
                    m["timestamp"] -= start_time

                # create empty list if type not already in dictionary
                if not type in r[k]:
                    r[k][type] = []

                # store metrics by type
                r[k][type].append(j[type])

    return (r, key_to_config)

def get_higher_ts(runs):
    """
    Find the higher timestamp of all runs.
    """
    higher = 0
    for run in runs:
        for metric in run:
            higher = max(higher, metric["timestamp"])

    return higher

def get_ts_bounds(run):
    """
    Find the first and last timestamp of a run.
    """
    first = run[0]["timestamp"]
    last = run[-1]["timestamp"]
    return (first, last)

def get_last_size(run):
    """
    Find the last size of a type.
    """
    return run[-1]["size"]

def create_metric(ts, size):
    """
    Create metric from timestamp and size.
    """
    metric = {}
    metric["timestamp"] = ts
    metric["size"] = size
    return metric

def assume_unknown_values(d):
    """
    Assume values for timestamps not reported.
    """

    for key in d:
        for type in d[key]:

            # find the higher timestamp of all runs for this type
            higher_ts = get_higher_ts(d[key][type])

            for run in d[key][type]:

                # find the first and last timestamp of this run
                (first_ts, last_ts) = get_ts_bounds(run)

                # find the last size of this run
                last_size = get_last_size(run)

                # create fake values from timestamp 0 to first_ts
                for i in range(0, first_ts):
                    metric = create_metric(i, 0)
                    run.insert(i, metric)

                # create fake values from last_ts to higher_ts
                for i in range(last_ts + 1, higher_ts + 1):
                    metric = create_metric(i, last_size)
                    run.append(metric)

    return d

def s(d):
    print(json.dumps(d, sort_keys=True, indent=2))

def main():
    d = get_metric_files()
    #s(d)
    (d, key_to_config) = group_by_config(d)
    #s(d)
    d = assume_unknown_values(d)
    #s(d)
    s(key_to_config)

main()
