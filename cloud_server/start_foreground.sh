#!/bin/bash

erlc *.erl && erl -s server start
