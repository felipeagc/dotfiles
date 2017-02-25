value=$(nvidia-settings -q CurrentMetaMode | grep -ic ForceCompositionPipeline=On)
if [ $value -eq 0 ]
then
    echo hello
    nvidia-settings --assign "CurrentMetaMode=DPY-5: nvidia-auto-select +0+0 { ForceFullCompositionPipeline = On }"
else
    nvidia-settings --assign "CurrentMetaMode=DPY-5: nvidia-auto-select +0+0 { ForceFullCompositionPipeline = Off }"
fi
