remote_file "/tmp/master.tar.gz" do
  source "https://github.com/snmsts/roswell/archive/master.tar.gz"
  mode "0644"
  not_if { File.exists?("/tmp/master.tar.gz") }
end

bash "tar xvf master.tar.gz" do
  user "root"
  cwd "/tmp"
  code %(tar xvf master.tar.gz)
  not_if { File.exists?("/tmp/roswell-master") }
end

bash "sh bootstrap" do
  user "root"
  cwd "/tmp/roswell-master"
  code %(sh bootstrap)
end

bash "./configure" do
  user "root"
  cwd "/tmp/roswell-master"
  code %(./configure)
end

bash "make" do
  user "root"
  cwd "/tmp/roswell-master"
  code %(make)
end

bash "make install" do
  user "root"
  cwd "/tmp/roswell-master"
  code %(make install)
end
