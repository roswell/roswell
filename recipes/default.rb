# encoding: utf-8
branch = "release"

remote_file "/tmp/#{branch}.tar.gz" do
  source "https://github.com/snmsts/roswell/archive/#{branch}.tar.gz"
  mode "0644"
  not_if { File.exists?("/tmp/#{branch}.tar.gz") }
end

bash "tar xvf #{branch}.tar.gz" do
  user "root"
  cwd "/tmp"
  code %(tar xvf #{branch}.tar.gz)
  not_if { File.exists?("/tmp/roswell-#{branch}") }
end

bash "sh bootstrap" do
  user "root"
  cwd "/tmp/roswell-#{branch}"
  code %(sh bootstrap)
end

bash "./configure" do
  user "root"
  cwd "/tmp/roswell-#{branch}"
  code %(./configure)
end

bash "make" do
  user "root"
  cwd "/tmp/roswell-#{branch}"
  code %(make)
end

bash "make install" do
  user "root"
  cwd "/tmp/roswell-#{branch}"
  code %(make install)
end
