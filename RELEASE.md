# Release
Bild jars:
```shell
./sbt clean cleanFiles "project importer-it" "project importer-isabelle" assembly "project findfacts" it:test "project importer-isabelle-build" "run -?"
```
Extract into archive:
```shell
tar -czf findfacts-0.4.4.tar.gz --transform 's,^,findfacts-0.4.4/,' \
  etc/components \
  importer-isabelle/etc \
  importer-isabelle/src \
  importer-isabelle/isa-lib \
  importer-isabelle-build/etc \
  importer-isabelle-build/src \
  importer-isabelle/isa-lib
```