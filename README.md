# create a git repo 
git init

# create a bare clone in shared directory
git clone --bare remoteness-index ../bare-remote/remoteness-index.git

# in the local repo check the remote is set and add, if not.
git remote -v
git remote add origin ../remoteness-index.git

# colleague can clone a copy of this repo
# git clone ../bare-remote/remoteness-index.git

# check if remote is set
git remote -v
git remote add origin ../remoteness-index.git

# both repos should be able to add and commit changes
git add .
git commit -m "commit message"
git push origin main

# and pull as usual
git pull origin main

# branches work as well but, 
# looking at remote, I don't see where the new branch is specified

# switch branches as usual
git checkout -b test

# commit changes and push 
git push origin test

# you may need to set the upstream branch
git push --set-upstream origin test



git checkout -b test
git pull origin test 

