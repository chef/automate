#!/bin/bash

curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate

./chef-automate airgap bundle create -c dev

airgapbundle=$(ls | grep automate-[0-9.]*aib) 
filename=$(echo $airgapbundle |  awk -F ".aib" '{print $1}')
gpg --armor --digest-algo sha256 --default-key 2940ABA983EF826A --output "$filename.asc" --detach-sign $airgapbundle
sha256sum $airgapbundle > "$filename.sha256sum"
aws s3 cp $airgapbundle "s3://chef-automate-artifacts/airgap_bundle/$VERSION/$airgapbundle.aib" --acl public-read --profile chef-cd





#aws s3 cp $airgapbundle "s3://sahiba-bucket1" --acl public-read --profile chef-cd
# gpg --armor --digest-algo sha256 --default-key 2940ABA983EF826A --output "automate-$VERSION.asc" --detach-sign "automate-$VERSION.aib"
# sha256sum "automate-$VERSION.aib" > "automate-$VERSION.sha256sum"
# aws s3 cp automate-$VERSION.aib "s3://chef-automate-artifacts/airgap_bundle/$VERSION/automate-$VERSION.aib" --acl public-read --profile chef-cd