[![MELPA](https://melpa.org/packages/tfsmacs-badge.svg)](https://melpa.org/#/tfsmacs)

`tfsmacs` is a package to work with Team Foundation Server from Emacs.
This package deals only with the source control aspect, not PBIs, sprints,
builds, or any other thing contained in TFS.

** BREAKING CHANGE NOTICE **: Instead of configuring a collection, you have to customize the workspace name now.
The original code used the collection, but the workspace works better as it also contains mappings information.

## Table of contents

<!--ts-->

   * [Installation](#installation)
     * [Prerequisites](#prerequisites)
     * [Install from MELPA](#install-from-melpa)
     * [Manual installation](#manual-installation)
     * [Configuration](#configuration)
   * [Manual](#manual)
     * [Target selection](#target-selection)
     * [Pending changes](#pending-changes)
     * [History](#history)
   * [Team Explorer Everywhere vs tf.exe](#team-explorer-everywhere-vs-tfexe)
   * [Special thanks](#special-thanks)
<!--te-->

## Installation

### Prerequisites

The first step is to get the "Team Explorer Everywhere Command Line Client" or "TEE CLC" from the [Team Explorer Everywhere releases page](https://github.com/Microsoft/team-explorer-everywhere/releases). More installation instructions can be found in the README for the repo.
Documentation specific to the TEE CLI can be a bit hard to find as most web searches return info on the regular TF.exe that is shipped with Visual Studio (to add to the confusion, both tools share a lot of commands). The reference specific to TEE CLI on MS Docs is [here](https://docs.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2010/gg413282(v=vs.100)).

**IMPORTANT: Run "tf eula" to accept the TEE license or the tool won't work!**

### Install from MELPA

You can find this package in MELPA. This is the recommended way to install tfsmacs.

### Manual installation

1. Install the package [tablist](https://github.com/politza/tablist) and then 
2. Place `tfsmacs.el` in your `load-path`.

### Configuration 

1. In your `.emacs` file:
```elisp
     (require 'tfsmacs)
     (setq tfsmacs-cmd  "location/of/TEE/tf")
     (setq tfsmacs-login "/login:domain\\userid,password")
```
2. There's a keymap provided for convenience, you can also bind the individual commands of course:
```elisp
     (global-set-key  "\C-ct" 'tfsmacs-map)
```

## Manual

All commands have pretty self-explanatory names. There are a few special modes for pending changes and history described below.

### Initial setup

If you have never worked with TFS before, the function `tfsmacs-setup-collection` will help you setup your workspace and mappings. Just follow the prompts.

### Target selection

Some commands alter their behaviour depending on the context:

* If you specify a filename, it is used.
* When called from a `dired` buffer, the marked files are used. If none are 
marked the file at point is used.
  * For some commands, only one file or two files can be marked, otherwise an error is 
raised
* The filename of the current buffer, if visiting a file
* When none of the above work, prompt for a file

This is relevant for the following commands: `tfsmacs-add`, `tfsmacs-checkout`,
`tfsmacs-history`, `tfsmacs-rename`, `tfsmacs-undo`.

### Pending Changes

People coming from Visual Studio are probably used to the Pending Changes
window. There's a somewhat similar implementation using 
tabulated-list-mode and tablist, `tfsmacs-pending-changes`.
It targets a directory, autocompleting with the `projetile` root if possible,
else it will autocomplete to the current directory of the buffer.
Once in the Pending Changes buffer you can:

* Mark and unmark files with the usual bindings m, u, etc.
* RET to visit the file at point
* R to Revert/undo changes on the files marked
* C to Check in the files marked.
* D to diff the version in your local with the latest on the server (using ediff)

### History

This is another tabulated-list derived mode, launched by `tfsmacs-history`. You can
request the history of a single file, or a directory. In the later case you will
get recursive history to all items under the dir.
The commands operate on the file under point, or the file marked if any. The 
defaults are as follows:

* Mark and unmark files as usual m, U, etc.
* C to open the changeset details for the item selected
* T ("This version"), to get the version selected
* D to diff the two versions marked (using ediff)

## Team Explorer Everywhere vs tf.exe

The reason the tool uses TEE CLI instead of the regular TF.EXE windows
client is that the later doesn't support associating a check in with a
work item (PBI). Most shops using TFS have this as a requirement.

One advantage of using this tool is that we can keep a process open which
seems to be much faster than calling the command each time, _except on the
first call_.

Retrieving file contents for diffs is synchronous, so those operations will block
Emacs. Alternatives are being evaluated, and suggestions are welcomed.

## Special thanks

Dino Chiesa ([GitHub profile](https://github.com/DinoChiesa)) wrote an integration
layer between Emacs and TFS a few years ago. You can still find his version
in EmacsWiki. He was kind enough to give me permission to take his code as
starting point for the current package. Without his initial package
and generosity tfsmacs as it is now wouldn't exist.

## Feedback appreciated!

This is my first Emacs package. Feedback and contributions are welcomed.


Contributors:
Sebastián Monía - http://github.com/sebasmonia
