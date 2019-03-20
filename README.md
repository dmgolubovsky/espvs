# espvs
## Espeak Vocal Studio

This GUI program helps modify voice parameters on the fly, generate a vocal sample and 
play it along with the selected backing track.

Program syntax as follows:
```
espvs /path/to/voice/file [ -s /path/to/score.musicxml ] [ -b /path/to/backing_track.wav ]
```

At this moment, score and backing track cannot be set from GUI, only from command line.

Once started, the program window looks as follows:
![main](https://github.com/dmgolubovsky/espvs/raw/master/docs/espvs-03202019.png "Main window")
When voice parameters are updated, the voice file is updated in place. There is no undo.
Press "Play" to generate the vocal sample; its name will be shown next to the score path. 
The generated vocal sample will start playing mixed with the backing track if provided. 
"Play" causes the sample to be played only once. "Loop" cycles between playback and regeneration 
of the vocal  sample, so if the sound is unsatisfactory, while the sample is playing, voice parameters
can be adjusted. When the sample finishes playing, in the loop mode, new vocal sample will be generated,
and played again.
