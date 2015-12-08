#!/bin/bash

function get_hash {
  git --git-dir ${2}/.git log --pretty=format:"+ ${1}: %s%n  %H" -n 1
}

JSCOQ_DIR=~/research/jscoq/
COQ_DIR=~/external/coq-git/
MC_DIR=~/external/coq/math-comp/
CM_DIR=${JSCOQ_DIR}/external/CodeMirror
FLOCQ_DIR=~/external/coq/flocq/
CT_DIR=~/external/coq/coquelicot/
TLC_DIR=~/external/coq/tlc/

function get_build_name {
  COQH=`git --git-dir ${COQ_DIR}/.git log --pretty=format:"%h" -n 1`
  JSCOQH=`git --git-dir ${JSCOQ_DIR}/.git log --pretty=format:"%h" -n 1`
  echo "Build: $JSCOQH/$COQH"
}

get_build_name

get_hash "coq"        $COQ_DIR
get_hash "jscoq"      $JSCOQ_DIR
get_hash "CodeMirror" $CM_DIR
get_hash "math-comp"  $MC_DIR
get_hash "flocq"      $FLOCQ_DIR
get_hash "coquelicot" $CT_DIR
get_hash "tlc"        $TLC_DIR

