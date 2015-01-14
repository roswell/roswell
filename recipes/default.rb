# encoding: utf-8
branch = "release"

bash "download and unzip roswell" do
  user "root"
  cwd "/tmp"
  code %(wget -O - 'https://github.com/snmsts/roswell/archive/#{branch}.tar.gz' | tar zxvf -)
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
