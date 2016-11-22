#!/usr/bin/env bash

# the github repo
REMOTE="git@github.com:DanielCardonaRojas/DanielCardonaRojas.github.io.git"
# the dir where hakyll generates site
SITE="_site"
# the dir for pushing to master branch
DEPLOY_DIR="deploy/"

print_info() {
  printf "  \e[1;34m $1\e[0m\n"
}
print_success() {
  printf "  \e[0;32mDONE: \e[0m $1\n"
}
print_error() {
  printf "  \e[0;31mERROR: \e[0m $1\n"
}
git_check() {
  git rev-parse || print_error "$PWD is already under git control"
}

dir_check() {
  if [ ! -f "site.hs" ]; then
    print_error "not at root dir"
    print_info "cd to root dir and rerun the script"
  fi
}

setup() {
  print_info "Begin setup"
  dir_check
  # remove the deploy dir if it already exists
  # create a new deploy dir
  rm -rf $DEPLOY_DIR
  mkdir $DEPLOY_DIR
  print_success "created empty $DEPLOY_DIR"

  # go inside, check there is no .git folder
  # then initialize git, adding github remote
  cd $DEPLOY_DIR
  git_check
  git init --quiet
  print_success "initialized git in $DEPLOY_DIR"
  git checkout --orphan master --quiet
  print_success "created master branch"
  git remote add origin $REMOTE
  print_success "added remote"
}

deploy() {
  dir_check

  # clean out $DEPLOY, generate $SITE,
  # then copy new files to $DEPLOY
  # rm -rf "$DEPLOY_DIR"/*
  print_info "building $SITE with hakyll"
  print_info "building site.hs"
  #ghc --make site.hs
  stack build
  print_success "built site.hs"
  print_info "generating site"
  #./site build > /dev/null
  stack exec decarojBlog rebuild
  print_success "site generated"
  cp -r "$SITE"/* $DEPLOY_DIR
  print_success "copied $SITE into $DEPLOY_DIR"

  # push to git
  print_info "creating commit message"
  COMMIT=$(git log -1 HEAD --pretty=format:%H)
  SHA=${COMMIT:0:8}
  print_success "commit message created"
  print_info "pushing files to master"
  cd $DEPLOY_DIR
  git add --all .
  git commit -m "generated from $SHA"
  git push origin master --force
  print_success "deployed site"
}

case "$1" in
  setup )
    setup;;
  deploy )
    deploy;;
  * )
    printf "USAGE: test.sh [setup | deploy]\n";;
esac
