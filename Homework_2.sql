--Lets create our database. This query was done on PostgresSql; There will be differences in notation
CREATE DATABASE amcViewers;

--I want to first import my data from my .CSV file. Let's create a table where each quizer gets their own 'account'
CREATE TABLE MovieViewer(
	ViewerID SERIAL PRIMARY KEY,
	Mone int NOT NULL,
	Mtwo int NOT NULL,
	Mthree int NOT NULL,
	Mfour int NOT NULL,
	Mfive int NOT NULL,
 	Msix  int NOT NULL,
	Age int NOT NULL,
	Like_Action varchar(3) NOT NULL,
	Favorite_Snack varchar(255) NOT NULL);
--Now lets transfer the data from the .csv to the table
COPY MovieViewer(Mone,Mtwo,Mthree,Mfour,Mfive,Msix,Age,Like_action,Favorite_Snack) FROM 'C:\Users\Public\Movies in theatres (Responses) - Form Responses 1.csv' DELIMITER ',' CSV HEADER;

--Now, I like to normalize my tables. Lets create a second table to insert our ratings
CREATE TABLE MovieRatings(
	RatingID SERIAL PRIMARY KEY,
	Movie1 int NOT NULL,
	Movie2 int NOT NULL,
	Movie3 int NOT NULL,
	Movie4 int NOT NULL,
	Movie5 int NOT NULL,
	Movie6 int NOT NULL);
--Transfer users' ratings onto the new table
INSERT INTO MovieRatings(Movie1,Movie2,Movie3,Movie4,Movie5,Movie6) SELECT Mone,Mtwo,Mthree,Mfour,Mfive,Msix FROM MovieViewer;

--Now we can alter the original table and drop thoses colmuns and create a new column to store Rating ID
ALTER TABLE MovieViewer DROP COLUMN Mone; 
ALTER TABLE MovieViewer DROP COLUMN Mtwo;
ALTER TABLE MovieViewer DROP COLUMN Mthree;
ALTER TABLE MovieViewer DROP COLUMN Mfour;
ALTER TABLE MovieViewer DROP COLUMN Mfive;
ALTER TABLE MovieViewer DROP COLUMN Msix;
ALTER TABLE MovieViewer ADD COLUMN RatingId int;

--Insert the ratings ids into the users' profiles and create a foriegn key that references the second table
UPDATE MovieViewer SET RatingId=(SELECT RatingID FROM MovieRatings WHERE MovieViewer.ViewerID=MovieRatings.RatingID); 
ALTER TABLE MovieViewer ADD FOREIGN KEY (RatingId) REFERENCES MovieRatings(RatingID);

--Our data is normalized and we can move to the R side of the homework :)
--We created a user to log in our database for connection purposes
CREATE USER rAcess WITH ENCRYPTED PASSWORD 'password';
GRANT ALL PRIVILEGES ON DATABASE amcviewers TO rAcess;


SELECT * FROM MovieViewer;
SELECT * FROM MovieRatings;
