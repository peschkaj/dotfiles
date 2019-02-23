#!/usr/bin/env bash

function vsync { nvidia-settings --assign CurrentMetaMode="nvidia-auto-select +0+0 { ForceFullCompositionPipeline = $@ }" ;}
[[ $(nvidia-settings -tq CurrentMetaMode | grep ForceFullCompositionPipeline=On) ]] && vsync Off || vsync On
