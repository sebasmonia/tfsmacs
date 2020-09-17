[![MELPA](https://melpa.org/packages/tfsmacs-badge.svg)](https://melpa.org/#/tfsmacs)

`tfsmacs` is a package to work with Team Foundation Server from Emacs.
This package deals only with the source control aspect, not PBIs, sprints,
builds, or any other thing contained in TFS.

**NOTICE: I'm not working daily with TFVC anymore. I will continue supporting the package, but if anyone is interested in taking over or just contributing new features, feel free to do so!**

**You can buy me a [cup of ko-fi](https://ko-fi.com/A0A527CN7)! There's also a [PayPal option](https://www.paypal.me/sebasmonia).**

## Table of contents

<!--ts-->

   * [Installation](#installation)
     * [Prerequisites](#prerequisites)
     * [Install from MELPA](#install-from-melpa)
     * [Manual installation](#manual-installation)
     * [Configuration](#configuration)
   * [Manual](#manual)
     * [Initial setup](#initial-setup)
     * [Getting help](#getting-help)
     * [Target selection](#target-selection)
     * [Pending changes](#pending-changes)
     * [History](#history)
     * [Shelvesets](#shelvesets)
   * [Team Explorer Everywhere vs tf.exe](#team-explorer-everywhere-vs-tfexe)
   * [Special thanks](#special-thanks)
<!--te-->

## Installation

### Prerequisites

The first step is to get the "Team Explorer Everywhere Command Line Client" or "TEE CLC"
from the [Team Explorer Everywhere releases page](https://github.com/Microsoft/team-explorer-everywhere/releases).
More installation instructions can be found in the README for the repo.
Documentation specific to the TEE CLI can be a bit hard to find as most web searches
return info on the regular TF.exe that is shipped with Visual Studio (to add to the confusion,
both tools share a lot of commands). The reference specific to TEE CLI on MS Docs
is [here](https://docs.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2010/gg413282(v=vs.100)).

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

### Initial setup

If you have never worked with TFS before, the function `tfsmacs-setup-workspace` will help you setup your
workspace and mappings. Just follow the prompts.
The next step is downloading code from the server, you can navigate the directories with `tfsmacs-server-directories`.
Once you have found the directory you are interested in, place the point over it and press "G".
This will create the necessary directories in your local environment and download the files.

I recommend you customize `tfsmacs-current-workspace` (if you used the included setup function, it takes
care of this step for you). If your organization is nuts (or really big) and you deal with several
collections, you can customize `tfsmacs-workspaces-alist` and then call `tfsmacs-switch-workspace` to
move between them easily.

### Getting help

Every function in the package has detailed documentation, you can use `describe-function` (C-h f) to
find out what they do. Let me know, or submit a pull request, if you find something lacking in the docs.

In every special buffer opened by tfsmacs you can use "h" to open a help pop up window that describes
the bindings available.

Finally, I recommend people who have never used TFS before to take a quick read at the docs. The objective
of tfsmacs is to cover most of the day to day operations, not everything the command line tool can do (even
if that's the long term goal). Also, the lingo in TFVC is different than that of most other similar tools
(check in = commit and undo = revert, for example).

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
`tfsmacs-delete`, `tfsmacs-get`,  `tfsmacs-history`, `tfsmacs-rename`, `tfsmacs-undo`.

### Pending Changes

People coming from Visual Studio are probably used to the Pending Changes
window. There's a somewhat similar implementation using 
tabulated-list-mode and tablist, `tfsmacs-pending-changes` (C-c t p).
It targets a directory, autocompleting with the `projetile` root if possible,
else it will autocomplete to the current directory of the buffer.
Once in the Pending Changes buffer use "h" to check out the available operations, 
which include check in, create shelves and diff against server version.

### History

This is another tabulated-list derived mode, launched by `tfsmacs-history` (C-c t h). You can
request the history of a single file, or a directory. In the later case you will
get recursive history to all items under the dir.
The commands operate on the file under point, or the file marked if any. In the
history buffer, use "h" to check out the available operations, which include get a
specific version, diff two versions marked and see the changeset details.

### Shelvesets

More tabulated-list modes! Use `tfsmacs-shelvesets` (C-c t s) to get a list of shelves from
the server. You can filter by owner (blank for only your shelves). The usual "h"
binding will show up all the operations available.  
You can create new shelves using the Pending Changes window to select the files
to include.

## Team Explorer Everywhere vs tf.exe

The reason the tool uses TEE CLI instead of the regular TF.EXE windows
client is that the later doesn't support associating a check in with a
work item (PBI). Most shops using TFS have this as a requirement.

One advantage of using this tool is that we can keep a process open which
seems to be much faster than calling the command each time, _except on the
first call_.

A second advantage is that the client is written in Java, so it's multi-platform.

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
