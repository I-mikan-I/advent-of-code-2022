#!/bin/bash

find . -name '*.rkt' -exec raco fmt -i {} \;