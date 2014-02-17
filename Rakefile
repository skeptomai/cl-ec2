#!/usr/bin/env ruby

# NOTE: the projects are in dependency order to make asdf-install work!
# Should find a better way
PROJECTS = [
  ["iso8601", 0.1],
  ["xml-schema-objects", 0.1],
  ["cl-ec2", 0.1],
  ["test-cl-ec2", 0.1],
]

BUILD_DIR_BASE = 'build'
INSTALL_BASE_PREFIX = "(asdf-install:install \"#{FileUtils.getwd}/build"
INSTALL_BASE_SUFFIX = "\")"
UNINSTALL_BASE_PREFIX = "(asdf-install:uninstall"
UNINSTALL_BASE_SUFFIX = ")"
LISP_INIT_COMMAND = "#!/Users/cbrown/Projects/lisptty/tty-lispworks -siteinit - -init ./lispworks-init.lisp"
#LISP_INIT_COMMAND =   "#!/usr/local/bin/sbcl --noinform --no-sysinit --userinit ./sbcl-init.lisp"

directory BUILD_DIR_BASE

task :default => [BUILD_DIR_BASE, :build_install_lisp]

task :status do
  sh %{ darcs whatsnew -l}
end

task :record do
  sh %{ darcs record -a }
end

task :push do
  sh %{ darcs push -a }
end

task :package => [:default]

task :build_install_lisp do
  File.open("install-packages.lisp",'w') do |ifile|

    ifile.puts LISP_INIT_COMMAND
    File.open("uninstall-packages.lisp",'w') do |ufile|
      ufile.puts LISP_INIT_COMMAND
      PROJECTS.each do |proj,version|
        ifile.puts "#{INSTALL_BASE_PREFIX}/#{proj}_#{version}.tar.gz#{INSTALL_BASE_SUFFIX}"
        ufile.puts "#{UNINSTALL_BASE_PREFIX} :#{proj} #{UNINSTALL_BASE_SUFFIX}"
      end
    end
  end
  File.chmod(0755,"install-packages.lisp")
  File.chmod(0755,"uninstall-packages.lisp")
end

PROJECTS.each do |proj|
  proj_name,version = proj[0..1]
  build_artifact = "#{BUILD_DIR_BASE}/#{proj_name}_#{version}.tar.gz"
  project_files = FileList["#{proj_name}/**/*"]
  file build_artifact  => project_files do |t|
    sh %{ tar czf #{t.name} #{proj_name} }
  end
  task :default => build_artifact
end

task :install =>[:build_install_lisp] do
  sh %{ ./install-packages.lisp }
end

task :uninstall =>[:build_install_lisp] do
  sh %{ ./uninstall-packages.lisp }
end

task :clean do
  [
    Proc.new {FileUtils.remove_dir BUILD_DIR_BASE},
    Proc.new {FileUtils.rm "./install-packages.lisp"},
    Proc.new {FileUtils.rm "./uninstall-packages.lisp"},
  ].each do |cleaner|
    begin
      cleaner.call
    rescue
    end
  end

  sh %{ find . -name "*~" -exec rm {} \\; }
  sh %{ find . -name "\#*" -exec rm {} \\; }
end

task :update do
  sh %{ darcs pull -a cb@virtual.augmento.net:ec2-common-lisp }
end

task :all => [:clean, :package, :install] do
end

=begin
class Task 
  def investigation
    result = "------------------------------\n"
    result << "Investigating #{name}\n" 
    result << "class: #{self.class}\n"
    result <<  "task needed: #{needed?}\n"
    result <<  "timestamp: #{timestamp}\n"
    result << "pre-requisites: \n"
    prereqs = @prerequisites.collect {|name| Task[name]}
    prereqs.sort! {|a,b| a.timestamp <=> b.timestamp}
    prereqs.each do |p|
      result << "--#{p.name} (#{p.timestamp})\n"
    end
    latest_prereq = @prerequisites.collect{|n| Task[n].timestamp}.max
    result <<  "latest-prerequisite time: #{latest_prereq}\n"
    result << "................................\n\n"
    return result
  end
end
=end

