#My Hakyll Blog


Hakyll is the Haskell version for the static site generator Jekyll based on Ruby.

The configuration process is quite hard and there is no standard way to do it. This blog in particular
uses stack and the stack template (hakyll-template).

Take a look at my [plost](https://danielcardonarojas.github.io/posts/configure_hakyll.html) on the subject for more information.


#Get it running

Install stack if you dont have it, from [here](https://docs.haskellstack.org/en/stable/README/)

```shell
stack build 
stack exec decarojBlog watch
```

#Commit to the source branch


```shell
#Review changes
git diff
git add .
#Or if just edited file al ready staged file
git add -u
git commit -m "updated source"
git push origin source
```


TODO
====

- [ ] Polish the workflow so the source branch gets updated when changed, or at least notify.
- [ ] Generate rss feed.



