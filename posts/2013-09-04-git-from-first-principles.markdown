---
title: Git from First Principles™
---

# What is git?

[Git] is a piece of software for managing snapshots of files. You want to take
snapshots when you are

- playing a game, to save your progress
- writing a large code, when you've finished writing a small part and tested it
- writing a book, when you've written a chapter
- et cetera

Basically, for any major human effort that can be saved on a computer, git lets
you

- create and save snapshots
- retrieve them
- record their history
- merge multiple snapshots into one
- share them with others

Git is just one of the many such softwares (called version control systems) out
there. 

# What is this tutorial about?

Git is generally perceived to be difficult to learn and use. Part of the
difficulty is in getting used to the idea of version control itself, but to some
extent, the huge number (100+) of git commands are to blame as well. It takes
some time to figure out which are the most useful commands and how they should
be used together to manipulate your snapshots the way you want (a.k.a. your "git
workflow"). Also, if you have used some other version control software before,
some amount of unlearning is needed to get comfortable with git. Thankfully, git
stores the snapshots in a straight-forward way so it is possible to develop a
mental picture of how each git command manipulates the snapshots. Nonetheless, some
"struggling through fundamental details" is necessary in [building intuition].
This tutorial is an attempt to accelerate the building of intuition.

Git's website has instructions on [installing git] and [setting it up] for the
first time. The rest of this tutorial assumes that you have a working git set up
and you know how to run git commands in a directory from the command line.

# Create and save snapshots
*git init, git add*

In the directory where you would like to create snapshots (let's call it `top`),
run `git init`. This creates a directory called `.git` in it. Now you can add
files in `top` and its sub-directories to git with `git add <filename>` and they
will get copied into `.git`. Behind the scenes, git implements some efficient
storage techniques. All files are stored in a compressed form. If you add two
files with the same content (or the same file twice), it will store only one
copy. Also, over time, each file will have many snapshots that only differ
slightly. In this case, git will pack those snapshots together efficiently. But
we need not concern ourselves with these details. Git would still work without
these optimizations - just that it would be very slow and take up a lot of
space. 

Another interesting optimization is that git generates 20-byte IDs for file
snapshots that depend purely on their contents. IDs for directory snapshots
(also 20 bytes) depends on IDs and names of files and subdirectories. As a
result, by simply comparing IDs, git can tell if two files or two directories
have the same content. This sounds a bit magical - how can you compare two files
without comparing every single byte, and how can you compare two directories
without comparing every single file? The truthful answer is that you cannot -
but git generates those IDs in such a way that it is very very unlikely that two
different files/directories get the same ID. You can check out [hash functions]
for more details. In theory, git can work by comparing files and directories
byte-by-byte, but that would make it impossibly slow.

![](/images/git-1.png)

Notice how in this figure, the directory structure is mirrored by git, except
that `file1` and `file3` point to the same snapshot because they have the same
contents "Hello World". The complete snapshot is uniquely identified by `id1`.
When we modify say `file2`, it will have to be saved in a new snapshot, say
`id5`. This would, in turn, change the `id2`, which would change `id1`. Thus, we
will end up with a totally new snapshot.

![](/images/git-2.png)

# Retrieve snapshots
*git commit, git checkout, git log*

Now we know how to create and store snapshots, but we don't really have a good
way to retrieve a snapshot. But, if we could save as a short description
of what's in the snapshot with the snapshot, we'll be able to list all commits
and figure out which we want. That is precisely what `git commit` does. It
packages the tree of the top-level directory with some meta information like
commit time, committer's name and email, and a descriptive commit message.

![](images/git-3.png)

Now you can list commits with `git log` and then checkout an old version of some
file with `git checkout -- <filename>`

# Record history of snapshots
*parent commits, HEAD, branches*

# Merge snapshots
*git merge*

# Share snapshots
*remote branches, git fetch*

# Common git workflows
*Atlassian*

[Git]: http://www.git-scm.com
[building intuition]: http://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy
[installing git]: http://git-scm.com/book/en/Getting-Started-Installing-Git
[setting it up]: http://git-scm.com/book/en/Getting-Started-First-Time-Git-Setup
[hash functions]: http://en.wikipedia.org/wiki/Hash_function
