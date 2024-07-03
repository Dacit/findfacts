# Release
Clean:
`rm **/isa-lib/*.jar`
Build jars:
```shell
./sbt -Dprofiles=memory,loader clean cleanFiles "project importer-isabelle-base" assembly "project findfacts" it:test
```
Extract into archive:
```shell
tar -czf findfacts-0.6.0.tar.gz --transform 's,^,findfacts-0.6.0/,' \
  etc/components \
  importer-isabelle-build/isa-lib \
  importer-isabelle-build/etc \
  importer-isabelle-build/src
```
Import index:
1. ssh-forward solr port `ssh -L 8983:localhost:8983 user@solrhost`
2. run importer `isabelle build_importer -C theorydata-${VERSION} -d '$AFP' -r localhost:8983 -i ${ISA_VERSION}_Isabelle${ISA_VERSION}_AFP${ISA_VERSION} -a`
3. rename new index to default (replaace leading `${ISA_VERSION}` with `default`)

Build and publish docker image:
```shell
docker login
./sbt "project search-webapp" "docker:publish"
```