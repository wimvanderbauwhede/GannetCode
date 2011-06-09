Assuming ghci is started in $GANNET_DIR/t/HelloWorld

:cd ../../Compiler
:set -cpp -optP-DWORDSZ=64
:load Main.hs
:set -fbreak-on-exception
:main -p -v -Y ../t/HelloWorld/hello.yml ./t/HelloWorld/hello.td
:trace
:hist



