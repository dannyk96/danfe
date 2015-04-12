#
#   RPM specfiel for DANFE
#

Summary: A comprehensive Finite Element Packages with built in pre and post processing
Name: danfe
Url: http://www.man.ac.uk/~zzcgudk/dansoft
Version: 3.2
Release: 1
Copyright: GNU GPL version 2
#License: gpl
Group: Applications/FiniteElements
#Source: %{name}-%{version}-src.tar.gz
Packager: Dr. Dan Kidger <dan@quadrics.com>
Requires: intel-ifc >= 5.0.1
#Prereq: 
Provides: danfe danplot danmesh
BuildRoot: /tmp/danfe
Prefix: /opt

%define prefix /opt/danfe

%define tng_build_dir $RPM_BUILD_DIR/%{name}-%{version}/tng
%define head_build_dir $RPM_BUILD_DIR/%{name}-%{version}/head
%define danbase /hom/dan/danfe5

BuildRoot: /home/dan/danfe5
#BuildRoot: /tmp
#installprefix: /tmp
#prefix:/home/dan/danfe3
#base:/home/dan/danfe3

%description
 Danfe is a fully featured Finite Element Package
 Danplot is a fully featured Finite Element Package

#%changelog
#*   August 2002   first build of an RPM
#%prep

#%setup

%build
# make ARCH=LINUX_IFC

%install
#  echo this is the %install script
  rm -rf $RPM_BUILD_ROOT
  mkdir -p $RPM_BUILD_ROOT%{prefix}
  cp -r /home/dan/danfe5/lib_src $RPM_BUILD_ROOT%{prefix}
  cp -r /home/dan/danfe5/src $RPM_BUILD_ROOT%{prefix}
  
#  cp 

%clean
rm -rf $RPM_BUILD_ROOT


%pre
echo This is Danfe\'s pre install script

%post
echo This is Danfe\'s post install script

%files
%{prefix}/src
%{prefix}/lib_src
