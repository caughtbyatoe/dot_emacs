# dot_emacs
This is my personal collection of Emacs related configuration files.  I have used Emacs as my primary editor and development environment for more than 25 years.  Despite this, my elisp skills are limited. I intentionally keep my customizations as simple as possible. I prefer to spend my time focused on my programming tasks - not fussing around with Emacs.  Honestly, many of the Emacs configurations I see make my head hurt.

My work is primarily focused on developing applications in C++.  This means I make heavy use of compilation (M-x compile), gdb (M-x gud-gdb), tags (M-.), vc (C-x v) and running shells (M-x shell).  The closer your workflow is to this, the more useful my configuration files may be to you.

I have several different modes within which I need to work:
  1. Emacs on Linux
  2. Emacs on MacOS
  3. Emacs on MacOS using Tramp Mode to work on a remote Linux server

I occasionally find myself using Emacs in terminal mode (i.e. emacs -nw).  But tramp mode has reduced the need for this substantially in the last few years.  I think gui Emacs is just way more efficient than the terminal.  Still, I spent the better part of a decade using the terminal mode exclusively and most of my configurations still reflect this.

I have varying degress of control and ability to install packages or newer versions of Emacs.  In other words, some of the machines I run Emacs on I own and/or have permission to install upgrades and others I do not.  I currently need to handle versions ranging from Emacs 24.3 to 27.2.

I have started experimenting with melpa on machines that I have permission to do so.  I'd love to get some experience with magit and lsp-mode but neither are universally available to me - at least not yet.

## dot_emacs.el and dot_emacs_macos.el
Since I do not share home directories between Linux and MacOS I have no need for a universal .emacs file that works in both environments.  Accordingly, I have a .emacs file that I use on Linux (dot_emacs.el) and one that I use on MacOS (dot_emacs_macos.el).  There is code duplication between the two files, but they are short enough that this doesn't really bother me much.


## Emacsclient in Tramp mode
Tramp mode is great.  In the use case where I must use one machine (a MacBook in my case) to develop code on another (a remote Linux server) tramp mode has replaced my previous workflow of running `emacs -nw` in a terminal connected to the remote machine via ssh.  Nearly all of my workflows work immediately via Tramp.  The one notable exception is emacsclient invocation from tramp mode shell buffers.

The essential problem is that the emacs server in default mode creates only a local machine socket connection.  This means that emacsclient invocations in tramp mode shell buffers (in my case a shell process running on a Linux server) cannot connect to a running server on my local machine (e.g. my MacBook).  This is a particular problem when making use of git commands (e.g. git commit, git rebase -i) that need to open an editor.

Two fix this, three essential things need to happen:
1. The server needs to start in the non-default "TCP" mode
2. Emacsclient invocations need access to a "server file" which tells emacsclient where/how to connect
3. Emacsclient invocations need to add a tramp-mode prefix to the file being edited.

### tcp_server.el
The tcp_server.el file adds a function `start-tcp-server` that can be used to start a server in TCP mode.  It interactively asks for the ip address that remote emacsclients should use to connect.

### ec_tramp.sh
This script should be used to invoke emacsclient from within tramp mode shell buffers. Setting the EDITOR environment variable (`export EDITOR=ec_tramp.sh`) is the easiest way to accomplish this.  The script may need editing so that the "server file" can be found by emacsclient.  If I'm fortunate enough to have a filesystem that is shared between the local and remote machines I usually set the server file location in tcp_server.el to match that in ec_tramp.sh.  If I'm not so lucky, then I need to copy the server file between machines.
