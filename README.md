Little utility for synchronizing one's [Paperless](http://crushapps.com/paperless/) lists on the iPhone/iPad with an [org-mode](http://orgmode.org) file via [Dropbox](https://www.dropbox.dom) and a local hg repo.

Paperless is an excellent little app that helps me keep my life in order.  It is, very simply, a list of lists that you can edit on your iPhone or iPad, which is nice for jotting down ideas, plans, todo items, etc. when you're on the go. You can set it up to sync with your Dropbox account so you can back up your lists and see them on another device using the same app or on any computer with an internet connection... sort of. 

The only problem with viewing the files in your Dropbox folder is that they are stored in xml, which isn't very pleasant to read or write. For me the most convenient way to edit lists on a computer is in emacs' org-mode.

That's where paperless-org comes in. It lets you load in the xml form of your lists from your local Dropbox folder into an org-mode buffer in emacs, and then write them back out again in xml every time you save the buffer. The result is a fairly seamless way to keep your lists in sync between iPhone/iPad and computer. 

As an aside, I also recommend turning the Paperless folder in your Dropbox into an hg repo, so you can get version control for your lists as well. This works well with the Paperless app, which simply ignores the .hg subdirectory.  I expect this works just fine with git too.