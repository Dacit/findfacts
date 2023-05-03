# Release
Clean:
`rm **/isa-lib/*.jar`
Build jars:
```shell
./sbt -Dprofiles=loader clean cleanFiles "project importer-isabelle-base" assembly "project findfacts" it:test "project importer-isabelle-build" "run -?"
```
Extract into archive:
```shell
tar -czf findfacts-0.5.0.tar.gz --transform 's,^,findfacts-0.5.0/,' \
  etc/components \
  importer-isabelle-build/isa-lib \
  importer-isabelle-build/etc \
  importer-isabelle-build/src
```