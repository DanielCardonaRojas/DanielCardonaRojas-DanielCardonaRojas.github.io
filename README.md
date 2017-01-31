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

Write a post or modify the templates, refresh the browser. After verifyin deploy html to master branch
doing `./deploy.sh deploy` and push the source.

> Note: ./deploy.sh deploy can be run with the source branch checkedout

#Commit to the source branch

Check out the source branch and push from there.

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
- [ ] Add another case to the deploy script so (./deploy.sh source) updates the source branch
- [ ] Generate rss feed.
- [ ] Add random art generator for banners.



