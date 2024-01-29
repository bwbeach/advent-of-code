#! /bin/bash
b2 sync --excludeRegex '.*' --includeRegex '.*input.txt' . b2://advent-inputs
