-- SELECT tag, ip, timestamp, min(ssi) as ssi  from sputnik  group by tag,timestamp,ip order by tag,timestamp;


-- CREATE VIEW view1_sputnik AS
--     SELECT sputnik.tag, sputnik.ip, sputnik."timestamp", min(sputnik.ssi) AS ssi FROM sputnik GROUP BY sputnik.tag, sputnik."timestamp", sputnik.ip ORDER BY sputnik.tag, sputnik."timestamp";

DROP TABLE ips IF EXISTS;
DROP TABLE tag_timestamp IF EXISTS;

CREATE TABLE tag_timestamp (
       id SERIAL PRIMARY KEY,
       tag INTEGER,
       timestamp TEXT
);


CREATE TABLE ips (
       tag_timestamp INTEGER REFERENCES tag_timestamp(id),
       ip CHARACTER VARYING(255) DEFAULT NULL::CHARACTER VARYING
--        ,FOREIGN KEY tag_timestamp REFERENCES tag_timestamp(id)
);



-- fills in tag_timestamp table
INSERT INTO tag_timestamp(tag, timestamp)
  SELECT DISTINCT sputnik.tag, sputnik.timestamp 
  FROM sputnik 
  GROUP BY sputnik.tag, sputnik.timestamp
  ORDER BY sputnik.tag, sputnik.timestamp;


-- fills in ips table
INSERT INTO ips
  SELECT DISTINCT id,ip 
  FROM tag_timestamp ts, sputnik s
  WHERE ts.tag = s.tag AND ts.timestamp = s.timestamp;

-- -- create output RDF from the ids table
-- SELECT  '_:b'||tt.tag||' :locatedIn "'||array_to_string(array_agg(ip), '; ')||'" . ["'||TIMESTAMP 'epoch' + CAST(tt.timestamp AS BIGINT)/1000 * INTERVAL '1 second'||'"]'
-- FROM tag_timestamp tt,
-- (SELECT tag_timestamp,ip from ips ORDER BY tag_timestamp,ip) AS ips
-- WHERE ips.tag_timestamp = tt.id
-- GROUP BY ips.tag_timestamp,tt.tag,tt.timestamp ;


-- create records for prolog
SELECT  'record('||tt.tag||','''||array_to_string(array_agg(ip), '; ')||''','||tt.timestamp||').'
FROM tag_timestamp tt,
(SELECT tag_timestamp,ip from ips ORDER BY tag_timestamp,ip) AS ips
WHERE ips.tag_timestamp = tt.id
ORDER BY tt.tag, ips
GROUP BY ips.tag_timestamp,tt.tag,tt.timestamp ;



CREATE TABLE tag_ips (
       id SERIAL PRIMARY KEY,
       tag INTEGER,
       ips TEXT,
       timestamp TEXT
);

-- collect all ips for a tag
INSERT INTO tag_ips(tag,ips,timestamp) 
  SELECT  tt.tag,array_to_string(array_agg(ip), '; '),tt.timestamp
  FROM tag_timestamp tt,
  (SELECT tag_timestamp,ip from ips ORDER BY tag_timestamp,ip) AS ips
  WHERE ips.tag_timestamp = tt.id
  GROUP BY ips.tag_timestamp,tt.tag,tt.timestamp ;


CREATE TABLE tag_ips_interval (
       tag INTEGER,
       ips TEXT,
       "start" TEXT,
       "end" TEXT
);

-- collect start and end (eliminate unneeded intervals)
INSERT INTO tag_ips_interval(tag,ips, "start")
SELECT n.tag, n.ips, n.timestamp
FROM tag_ips c, tag_ips n
WHERE     c.id+1 = n.id
      AND (c.tag != n.tag OR c.ips != n.ips);


INSERT INTO tag_ips_interval(tag,ips, "end")
SELECT c.tag, c.ips, c.timestamp
FROM tag_ips c, tag_ips n
WHERE     c.id+1 = n.id
      AND (c.tag != n.tag OR c.ips != n.ips);
