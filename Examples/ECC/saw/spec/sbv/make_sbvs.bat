rmdir /s generated
mkdir generated
cryptol +RTS -H128m -K64m -RTS -b make_sbvs.cmd
