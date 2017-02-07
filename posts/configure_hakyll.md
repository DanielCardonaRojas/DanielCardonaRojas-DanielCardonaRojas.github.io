---
title: Hakyll Configuration for Github Pages
date: 2016-11-20
---
It seems a bit hard to get Hakyll setup for github pages and like most other blogs made with Hakyll
one of the first posts is about how to set it up.

Working with hakyll is pretty easy. What is a little confusing is how to get git and github setup.

The challange is to setup a remote repository in github with two independent branches, the master branch will containt all of your html generated files
as well as javascripts, css and images.  Another branch (in this case named source) will have the haskell source and templates for generating the 
html. So we'll want to automate this with deployment script that will switch to the right branch, push the right content etc.

There are many tutorials teaching how to get started and setup git the one that worked for me was 
[this](http://jameslawson.github.io/posts/2015-02-23-hakyll-and-github-pages.html), the first link below.

- [jameslawson](http://jameslawson.github.io/posts/2015-02-23-hakyll-and-github-pages.html)
- [Official tutorial](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html)

# Prerequisites

Install stack

# stack setup

With Hakyll there are two options (cabal and stack), in this tutorial I used a stack template instead. 

Getting started is really easy: 

```shell
stack new <blogName> hakyll-template
```


Build and get live automatically updated preview of your site:

```shell
stack build 
stack exec <blogName> watch
```

You can just regenerate all html by issuing: 

```shell
stack rebuild
# Or 
stack clean && stack build
```

# Design your blog

Write a post and get a hang of how things work in Hakyll, see how  it gets render automatically with the `watch` command play with the frontmatter options.

In this blog I used a template taken from: [hakyll-cssgarden](https://github.com/katychuang/hakyll-cssgarden). 
Clone and copy the templates, site.hs and static folder. I prefer its folder structure than the one the default template has.

# Git/Github  setup

If your new to github pages like I was, to setup a github page youll have to create a repository with the following name:

> your_github_name.github.io

**Setup SSH key** 

Setting up an ssh key is important so you dont get prompted with a user and password every time how push a commit.

Follow [this](https://help.github.com/articles/adding-a-new-ssh-key-to-your-github-account/) guide to se the detailed process.

**Create a .gitignore file ** 

This should have something like this:

```yaml
.stack-work/
site
_site
_cache
deploy
.DS_STORE
.ghci
```

**Create the source branch**

This is only done the first time. Create a source branch and do an initial commit:

```shell
git init
#git add the origin
git remote add origin git@github.com:yourGithubName/yourGithubName.github.io.git
#Create and switch to source branch
git checkout -b source
#Make sure that ignored files dont get tracked
git status
# If ok add all files.
git add -A
git commit -m "First source commit"
git push origin source
```

# Edit the deploy script

Copy the deploy.sh script from [here](https://github.com/DanielCardonaRojas/DanielCardonaRojas.github.io/blob/source/deploy.sh) this is 
a slightly modified version of the script from the first tutorial listed above. I just modfied it to use the  stack command instead of ghc. 

1. Change `REMOTE="git@github.com:DanielCardonaRojas/DanielCardonaRojas.github.io.git"` to match your own github page.
2. Edit this line `stack exec decarojBlog rebuild` to match the name of your the stack genereted binary, which can be changed from the blogName.cabal

After modifying the script do this the first time:

```shell
chmod 700 deploy.sh
./deploy setup
```


# Workflow

Write a blog push it to your master branch doing:  `./deploy.sh deploy`. This can be done if your in the source or master branch.
Note that this will only update the master branch i.e your html site. Updating of source branch must be done manually, like this:

```shell
git status
git add .
git commit -m "Updated source"
git push origin source
```

You can start writing a post and only update source until it ready for being published

#Fix Cache Problems

You might get this message `(snapshot content) was not found in the cache` just erase "_cache" contents.



# Extras

Add an RSS feed.
Add syntax highlighting.
