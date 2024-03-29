---
src_dir: ./src
         ./libsrc
output_dir: ./ford_doc
exclude: qp_opengl.F
graph: true
project_github: https://github.com/dannyk96/danfe
project_website: http://github.com
summary:  A 3D finite Element Package
author: Dr. Daniel Kidger
author_description: A former IBMer who writes Finite Element software in his spare time
github: https://github.com/dannyk96
email: daniel.kidger@gmail.com
linkedin: https://www.linkedin.com/in/danielkidger/
author_pic: https://avatars.githubusercontent.com/u/5859830?v=4
predocmark: >
---

@Note
You can include any notes (or bugs, warnings, or todos) like so.


# DANFE 
## A general putrpose 2d/3d Finite Element analysis suite
This is the source tree for DANFE - a suite of finite element software

Contact:  daniel.kidger@alumni.manchester.ac.uk or daniel.kidger@gmail.com.

The code is freely available for non-commerical use.

## List of applications
- DANFE The main Finitelment analysis engine
- DANPLOT A  Interactive Finite Element Visualisation package
- DANMESH A Structured rule-based mesh generator
- DANFRONT a 2d Unstructuctured mesh generator
Additional applications
- DANQP A quick 2d mesh drawing tool (uses pgplot to do X11 graphics)
- DANMUNG A Converter between several mesh file formats


## Example usage

```
$ danfe -v ex1_slope.d
$ danplot slope.out
```


## TODO

## Contact details

Dr. Dan Kidger
danial.kidger@gmail.com
15/09/2022


