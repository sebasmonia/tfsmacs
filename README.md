# tfs.el
Emacs package for interaction with TFS source control.

## Synopsis

`tfs.el` is package to work with Team Foundation Server from Emacs.
This package deals only with the source control aspect, not PBIs, sprints,
builds, or any other thing contained in TFS.

## Dependencies

 * [Team Explorer Everywhere CLI](https://github.com/Microsoft/team-explorer-everywhere/releases).
 * [tablist](https://github.com/politza/tablist)

## Quickstart

The package is not yet in MELPA (coming soon!!!).

1. Install dependencies (see above)
2. Place `tfs.el` in your `load-path'.
3. In your `.emacs` file:
```elisp
     (require 'tfs)
     (setq tfs-cmd  "location/of/TEE/tf")
     (setq tfs-login "/login:domain\\userid,password
           -or-
     (setq tfs-login (getenv "TFSLOGIN"))
```
4. Set local or global key bindings for tfs commands. For example:
```elisp
     (global-set-key  "\C-ctp" 'tfs-pending-changes)
     (global-set-key  "\C-cto" 'tfs-checkout)
     (global-set-key  "\C-cti" 'tfs-checkin)
     (global-set-key  "\C-ctr" 'tfs-rename)
     (global-set-key  "\C-ctg" 'tfs-get)
     (global-set-key  "\C-ctd" 'tfs-get-recursive)
     (global-set-key  "\C-cth" 'tfs-history)
     (global-set-key  "\C-ctc" 'tfs-changeset)
     (global-set-key  "\C-ctu" 'tfs-undo)
     (global-set-key  "\C-ct-" 'tfs-delete)
     (global-set-key  "\C-ct+" 'tfs-add)
```

## Some notes about the commands

### Target selection

Some commands alter their behaviour depending on the context:

* If you specify a filename, it is used
* When called from a `dired` buffer, the marked files are used. If none are 
marked the file at point is used.
  * For some commands, only one file can be marked, otherwise an error is 
raised
* The filename of the current buffer, if visiting a file
* When none of the above work, prompt for a file

This is relevant for the following commands: `tfs-add`, `tfs-checkout`,
`tfs-delete`, `tfs-get`,`tfs-get-recursive` (directory instead of file),
`tfs-history`, `tfs-rename`, `tfs-undo`.

### Pending Changes

People coming from Visual Studio are probably used to the Pending Changes
window. There's a somewhat similar implementation using 
tabulated-list-mode and tablist, `tfs-pending-changes`.
It targes a directory, autocompleting with `projetile` if possible,
else it will autocomplete to the current directory of the buffer.
Once in the Pending Changes buffer you can:

* Mark and unmark files with the usual bindings m, u, etc.
* RET to visit the file at point
* R to Revert/undo changes on the files marked
* C to Check in the files marked.

## Team Explorer Everywhere vs tf.exe

The reason the tool uses TEE CLI instead of the regular TF.EXE windows
client is that the later doesn't support associating a check in with a
work item (PBI). Most shops using TFS have this as a requirement.

One advantage of using this tool is that we can keep a process open which
seems to be much faster than calling the command each time, _except on the
first call_.

## Special thanks

Dino Chiesa ([GitHub profile](https://github.com/DinoChiesa)) wrote an integration
layer between Emacs and TFS a few years ago. You can still find this version
in EmacsWiki. He was kind enough to give me permission to take his code as
starting point for the current package. Without his initial package
and generosity tfs.el as it is now wouldn't exist.

## Feedback appreciated!

This is my first Emacs package. Feedback and contributions are welcomed.


Contributors:
Sebastián Monía - http://github.com/sebasmonia
