library(neo4r)

# Docker setup that worked for me
# system("docker create --publish=7474:7474 --publish=7687:7687 --env=NEO4J_AUTH=neo4j/password --volume=$HOME/neo4j/data:/tmp/neo4j -v $HOME/neo4j/import:/var/lib/neo4j/import --name neo4j_container neo4j")
# system("docker start neo4j_container")



system("docker stop neo4j")
system("docker rm neo4j")
system("docker run --name neo4j --env NEO4J_AUTH=neo4j/password --publish=7474:7474 --publish=7687:7687 -d neo4j")

con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "neo4j",
  password = "password"
)

con$ping()
con$get_version()
con$get_constraints()
con$get_labels()
con$get_relationships()
con$get_index()

str(con)

play_movies() %>%
  call_neo4j(con)


# so for a tabular database, i just need to point to the SQLite file

system("docker stop neo4j && sleep 2 && docker rm neo4j")
