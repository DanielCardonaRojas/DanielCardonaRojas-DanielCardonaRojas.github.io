#!/usr/bin/env bash

#The syntax for executing should be: ./deploy.sh source commit <optional_message> OR ./deploy.sh site 

# the github repo
REMOTE="git@github.com:DanielCardonaRojas/DanielCardonaRojas.github.io.git"
# the dir where hakyll generates site
SITE="_site"
# the dir for pushing to master branch
DEPLOY_DIR="deploy/"

# The name of the stack generated proyect, or more specifically the executable program name.
PROYECT_NAME="decarojBlog"

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

#UPDATE SOURCE BRANCH

commit(){
    commit_message="$1"
    git checkout source
    #Review changes
    git status
    read -p "Do you want to commit all changes? [y/n] " -n 1 -r
    echo    # (optional) move to a new line
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        echo "Preparing push to source branch"
        git add .
        echo "Commiting with message $commit_message"
        git commit -m "$commit_message"
        git push origin source
    else 
        exit 1
    fi

}

function usage ()
{
    echo "Usage :  $0 [deploy | setup | commit [-m <commit message>]]"

}    # ----------  end of function usage  ----------

#======================== Argument parsing / Main ====================

case "$1" in

  #SETUP REPO
  setup )
    setup;;

  #DEPLOY SITE I.E HTML AND STATIC RESOURCES
  deploy )
    deploy;;

  #UPDATE SOURCE CODE AND POSTS MARKDOWN
  commit )

        shift
        echo "updateing $1 ${opt}"
        if [ -z "$1" ]; then
                echo "No option especified"
                usage
                exit
        fi

        while getopts ":m:" opt
        do
        case ${opt} in

          m|message )  
              commit "$OPTARG"
              exit 0 ;;

          \? )
              echo "Invalid Option: -$OPTARG requires an argument" 1>&2
              exit 1
              ;;
          : )
              echo "Invalid Option: -$OPTARG requires an argument" 1>&2
              exit 1
              ;;

          * )  echo -e "\n  Option does not exist : $OPTARG\n"
                usage; exit 1;;

        esac    # --- end of case ---
        done
        shift $((OPTIND -1))
        ;;

  * )
    usage;;
esac
