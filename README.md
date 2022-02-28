# df-log-viewer-h

Convenient (hopefully) viewer for announcements of 
[Dwarf Fortress game](http://www.bay12games.com/dwarves/) .

At the time, almost everything is ready, except for reencoding of some characters,
and many parsing rules are not done.

The app heavily relies on Francisco Vallarino [Monomer framework](https://github.com/fjvallarino/monomer), and 
in principle, it can run on Windows, Linux and macOS.

## Installation

On windows you can use [this binary](https://drive.google.com/file/d/11teNPY7dQbf0PWJwYPqMBFyVDcfpCRPL/view?usp=sharing).

To build from source, follow similar 
[instructions](https://github.com/fjvallarino/monomer/blob/main/docs/tutorials/00-setup.md) 
for Monomer.

Folder with binary and configaration files can be placed in 
PeridexisErrant's Starter Pack utilities folder. 

Alternatively, you can specify the path to gamelog.txt in dflv.yaml
or on the command line of the program dflv(.exe).


## Fonts

You can specifiy your own fonts; place them in assets folder,
and specify fonts' filenames in dflv.yaml. By default, we use 
[Roboto](https://fonts.google.com/specimen/Roboto) fonts family,
licensed under the [Apache license](http://www.apache.org/licenses/LICENSE-2.0).

Correctness of word wrapping in log windows for other fonts is not tested.

