DROP DATABASE IF EXISTS Transmission_Loss;
CREATE DATABASE IF NOT EXISTS Transmission_Loss;

USE Transmission_Loss;

DROP TABLE IF EXISTS `Fields`;
CREATE TABLE IF NOT EXISTS `Fields`(
  idField INT (10) UNSIGNED NOT NULL AUTO_INCREMENT,
  IsCalculated BIT (1) NOT NULL,
  Latitude_degrees FLOAT NOT NULL,
  Longitude_degrees FLOAT NOT NULL,
  SourceDepth_meters FLOAT NOT NULL,
  VerticalBeamWidth_degrees FLOAT NOT NULL,
  VerticalLookAngle_degrees FLOAT NOT NULL,
  LowFrequency_Hz FLOAT NOT NULL,
  HighFrequency_Hz FLOAT NOT NULL,
  MaxCalculationDepth_meters FLOAT NOT NULL,
  Radius_meters INT (6) UNSIGNED NOT NULL,
  DataDirectoryPath TEXT NOT NULL,
  BinaryFileName TEXT,
  RangeAxisBytes BLOB,
  DepthAxisBytes BLOB,
  PRIMARY KEY (idField)
/*  KEY `IsCalculatedIndex` (`IsCalculated`),*/
/*  KEY `DataDirectoryIndex` (`DataDirectoryPath`(256)) */
)
ENGINE = MYISAM DEFAULT CHARSET = latin1;

DROP TABLE IF EXISTS `Radials`;
CREATE TABLE IF NOT EXISTS `Radials`(
  idRadial INT (10) UNSIGNED NOT NULL AUTO_INCREMENT,
  idField INT (10) UNSIGNED,
  IsCalculated BIT (1) NOT NULL,
  CalculationStarted DATETIME DEFAULT 0,
  CalculationFinished DATETIME DEFAULT 0,
  BearingFromSource_degrees FLOAT NOT NULL,
  BellhopConfiguration TEXT NOT NULL,
  BottomProfile TEXT NOT NULL,
  PRIMARY KEY (idRadial)
)
ENGINE = MYISAM DEFAULT CHARSET = latin1;
