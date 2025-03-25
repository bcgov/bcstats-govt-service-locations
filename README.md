https://www.saintsjd.com/2011/01/what-is-a-bare-git-repository/

# the remote for this is a bare clone created with:
# create a bare clone in shared directory
git clone --bare remoteness-index 'G:/Operations/Data Science and Analytics/2025 Government Service Locations/remoteness-index.git'

# you can clone a copy of this repo
git clone 'G:/Operations/Data Science and Analytics/2025 Government Service Locations/remoteness-index.git'

# check if remote is set
git remote -v
git remote add origin 'G:/Operations/Data Science and Analytics/2025 Government Service Locations/remoteness-index.git'

# you should be able to add and commit changes
git add .
git commit -m "commit message"
git push origin main

# and pull as usual
git pull origin main

# branches work as well but...
# looking at remote I don't see where the new branch is specified

# switch branches as usual
git checkout -b test

# commit changes and push 
git push origin test
git pull origin test 

# you may need to set the upstream branch
git push --set-upstream origin test


