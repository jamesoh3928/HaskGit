#!/bin/bash

cd ..
#############################Test git show######################################
# sudo cp -r .git/ .haskgit/
# echo $(stack exec haskgit-exe -- show "c00f8ed5a6e908e370e6be83cb6837255bcb2cad") 
# echo $(git show "c00f8ed5a6e908e370e6be83cb6837255bcb2cad")

for head in $(ls .git/objects | grep -vE 'info|pack')
do
    for obj in $(ls .git/objects/$head)
    do
        echo $head$obj
        hgOut=$(stack exec haskgit-exe -- show "$head$obj")
        gitOut=$(git show $head$obj)
        if ["$hgout" == "$gitout"]; then
            echo "yea"
        else
            echo $(diff hgOut gitOut)
        fi
    done
done

#############################gitHashCommand#####################################
# TBD not sure if git has this command

#############################githashObject######################################

# Error
# ❯ stack exec haskgit-exe -- hashobject HaskGit.cabal                  HaskGit/git/test !
# haskgit-exe: Codec.Compression.Zlib: compressed data stream format error (incorrect header check)

#############################gitUpdateRef#######################################

# TBD no use case in Experiment.hs

cd test