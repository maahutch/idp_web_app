from neo4j.v1 import GraphDatabase
import pandas as pd

uri = "bolt://156.56.32.126:7687"

file_object = open("pswd.txt", mode = 'r')

pwd = file_object.readline()

pwd = pwd.rstrip('\n')

driver = GraphDatabase.driver(uri, auth=("neo4j", pwd))


#Get Categories for menu
def get_cat_py(lev):
  with driver.session() as session:
    query_string = "MATCH (a:category{level:'%s'}) RETURN DISTINCT(a.category) AS cat" % lev
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)
  
 
#Query for getStart function
def get_start_py(cat):
  with driver.session() as session:
    query_string = "MATCH (a:category{category: '%s'})-[:PART_OF]-(b:organization{opioid_involved:'1'})" \
                   " RETURN b.org_name AS org_name, toFloat(b.latitude) AS latitude, toFloat(b.longitude) AS longitude, a.level AS level, a.category AS category" % cat
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)


#Query for getNext function
def get_next_py(name, lev, dist):
  with driver.session() as session:
    query_string = """MATCH (a:organization{org_name: '%(name)s' })-[]-(b:category)-[:TRANSITIONS_TO]-(c:category{level: '%(lev)s' })-[]-(d:organization{opioid_involved:'1'}) 
                      WITH point({longitude:toFloat(a.longitude), latitude:toFloat(a.latitude)}) AS first, 
                         point({longitude:toFloat(d.longitude), latitude:toFloat(d.latitude)}) AS finish, 
                         a.latitude AS start_lat, 
                         a.longitude AS start_lon, 
                         d.latitude AS latitude, 
                         d.longitude AS longitude, 
                         d.org_name AS org_name, 
                         c.level AS level, 
                         c.category AS category 
                         WHERE distance(first, finish) < %(dist)f
                         RETURN 
                         DISTINCT(org_name), 
                         start_lat,
                         start_lon,
                         latitude,
                         longitude,
                         level,
                         category, 
                         distance(first, finish) AS dist
                         ORDER BY dist ASC
                         LIMIT 5
                         """ % {'name':name, 'lev':lev, 'dist':dist}
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)
  
  
#All opiate orgs query for all.oi function
def get_all_oi_py():
  with driver.session() as session: 
    query_string = "MATCH (n:organization{opioid_involved:'1'}) RETURN toFloat(n.latitude) AS latitude, toFloat(n.longitude) AS longitude, n.org_name AS org "
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)

#All ed orgs query for all.oi function
def get_all_ed_py():
  with driver.session() as session: 
    query_string = "MATCH (n:organization{workforce_involved:'1'}) RETURN toFloat(n.latitude) AS latitude, toFloat(n.longitude) AS longitude, n.org_name AS org "
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)                      
                         
        
#Query for get.oi.org function for oi orgs
def get_oi_org_py():
  with driver.session() as session: 
    query_string = """MATCH (n:organization{opioid_involved:'1'})-[:PART_OF]-(b:category)
                      WHERE  b.category <> ''
                      RETURN DISTINCT(n.org_name) AS org"""
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df) 
  
  
#Query for get.oi.org function for oi orgs
def get_ed_org_py():
  with driver.session() as session: 
    query_string = """MATCH (n:organization{workforce_involved:'1'})-[:PART_OF]-(b:category)
                      WHERE  b.category <> ''
                      RETURN DISTINCT(n.org_name) AS org"""
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df) 
  

#Query for get.cat.combo function
def get_cat_combo_py(orgNm2):
  with driver.session() as session:
    query_string = """MATCH (n:organization{org_name: '%s'})-[]-(a:category)
                      WHERE a.category <> 'NA'
                      AND   a.category <> 'NULL'
                      RETURN DISTINCT(a.category) AS cat,
                                      n.org_name  AS org_name,
                                      n.latitude  AS lat,
                                      n.longitude AS long """ %orgNm2
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df) 



#Query for get.source.org function
def get_source_org_py(orgNm2):
  with driver.session() as session: 
    query_string = """MATCH (n:organization{org_name: '%s'})-[]-(a:category)
                      WHERE a.category <> 'NA'
                      AND   a.category <> 'NULL'
                      RETURN DISTINCT(a.category) AS cat,
                                      n.org_name  AS org_name,
                                      n.latitude  AS lat,
                                      n.longitude AS long""" %orgNm2
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)                                  


#Query for get.match.org.oi for opiates
def get_mat_org_oi_py(allPattern, allWhere, allReturn):
  with driver.session() as session: 
    query_string = """MATCH %(allPattern)s 
                      WHERE %(allWhere)s 
                      AND  a.opioid_involved =  '1'
                      RETURN a.org_name as org, %(allReturn)s,
                      toFloat(a.latitude) AS latitude, toFloat(a.longitude) AS longitude""" % {'allPattern': allPattern, 'allWhere': allWhere, 'allReturn': allReturn}
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)                                  

#Query for get.match.org.oi for education
def get_mat_org_ed_py(allPattern, allWhere, allReturn):
  with driver.session() as session: 
    query_string = """MATCH %(allPattern)s 
                      WHERE %(allWhere)s 
                      AND  a.workforce_involved =  '1'
                      RETURN a.org_name as org, %(allReturn)s,
                      toFloat(a.latitude) AS latitude, toFloat(a.longitude) AS longitude""" % {'allPattern': allPattern, 'allWhere': allWhere, 'allReturn': allReturn}
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df) 
  
  
#make_net queries - oi
def get_net1_py():
  with driver.session() as session: 
    query_string = """MATCH (a:organization)-[r:REFERRED_TO]->(b:organization)
                      WHERE a.opioid_involved = '1'
                     RETURN a.blended_id       AS From, 
                            b.blended_id       AS To,
                            a.org_name         AS name1, 
                            b.org_name         AS name2,
                            a.naicsmap         AS from_group, 
                            b.naicsmap         AS to_group
                  """
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)
  
def get_net2_py():
  with driver.session() as session: 
    query_string = """MATCH (a:organization)-[:MEMBER_OF]->(b:coalition)
                      WHERE  a.opioid_involved = '1'
                      RETURN a.blended_id      AS From, 
                             b.name            AS To,
                             a.org_name        AS name1, 
                             b.full_name       AS name2, 
                             a.naicsmap        AS from_group
                  """
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df) 

def get_net3_py():
  with driver.session() as session: 
    query_string = """MATCH (a:organization)-[:COLLABORATES_WITH]->(b:organization)
                      WHERE  a.opioid_involved = '1'
                      RETURN a.blended_id       AS From, 
                             b.blended_id       AS To,
                             a.org_name         AS name1, 
                             b.org_name         AS name2,
                             a.naicsmap         AS from_group, 
                             b.naicsmap         AS to_group
                  """
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)
                     

#make_net queries - ed      
def get_net_py():
  with driver.session() as session: 
    query_string = """MATCH (a:organization)-[:MEMBER_OF]->(b:program)
                      WHERE a.workforce_involved = '1'
                      RETURN a.blended_id      AS From, 
                             b.name            AS To,
                             a.org_name        AS name1, 
                             b.name            AS name2,
                             a.group           AS from_group
                  """
    result = session.run(query_string)
    df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)
  
  
#make_net unconn - oi
def all_oi_py():
  with driver.session() as session: 
   query_string = """MATCH (a:organization)
                     WHERE  a.opioid_involved = '1'
                     RETURN a.blended_id AS id,
                     a.org_name   AS label
                  """
   result = session.run(query_string)
   df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df) 
  
#make_net unconn - ed
def all_ed_py():
  with driver.session() as session: 
   query_string = """MATCH (a:organization)
                     WHERE  a.workforce_involved = '1'
                     RETURN a.blended_id AS id,
                     a.org_name   AS label
                  """
   result = session.run(query_string)
   df = pd.DataFrame([r.values() for r in result], columns = result.keys())
  return(df)  
  
  
  


         
