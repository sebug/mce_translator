MCE Translator
==============
This projects aims to provide an easy way to put new translations into
TinyMCE from a simple tabular format.

It also serves for me to explore some ideas linked to zippers ( http://en.wikipedia.org/wiki/Zipper_(data_structure) ).

To read the CSV input, it uses [FSharp.Data](http://fsharp.github.io/FSharp.Data/). It can be installed using NuGet ([See the instructions for MacOS X nuget use](http://orientman.wordpress.com/2012/12/29/for-the-record-how-to-run-nuget-exe-on-os-x-mountain-lion/)).

To install the FSharp.Data package:

mono nuget.exe install FSharp.Data

To build:

fsharpc -r FSharp.Data.1.1.10/lib/net40/FSharp.Data.dll mce_translator.fs

To run, mono will have to find the FSharp.Data dll. As I didn't want to put
it in the gac, I just used

export MONO_PATH=$MONO_PATH:~/Documents/Projets/mce_translator/FSharp.Data.1.1.10/lib/net40

to tell it where to find it. So now I can just run

mono mce_translator.exe example_input.csv out





