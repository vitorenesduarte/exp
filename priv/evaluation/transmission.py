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
    Return a dictionary from run (timestamp) to list of metric files.
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
    
    for dir in d:
        config_path = os.path.join(dir, CONFIG_FILE)
        (start_time, k) = key(
            read_json(config_path)
        )

        # create empty list if key not already in dictionary
        if not k in r:
            r[k] = []

        for file in d[dir]:
            # store start time and file name
            p = (start_time, file)
            r[k].append(p)

    return r

def s(d):
    print(json.dumps(d, sort_keys=True, indent=2))

def main():
    d = get_metric_files()
    s(d)
    d = group_by_config(d)
    s(d)

main()
