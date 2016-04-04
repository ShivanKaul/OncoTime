var mysql = require('mysql');
var Table = require('cli-table');
var db = mysql.createConnection({
	host: 'localhost',
	user: '520student',
	password: 'comp520',
	database: 'oncodb',
	port: 33306
});
db.connect(function(err) {
	if (err) console.log(err);
	else {
		db.query('select * from Patient where  (Patient.DateOfBirth = 1951 OR Patient.DateOfBirth = 1952 OR Patient.DateOfBirth = 1953 OR Patient.DateOfBirth = 1954 OR Patient.DateOfBirth = 1955) AND  (Patient.Sex like "m%")', function(err, rows, fields) {
			if (err) throw err;
			console.log(display(rows).toString());
		});
	}

		db.query('/*doctors filtering hasn't been implemented yet, sorry! */', function(err, rows, fields) {
			if (err) throw err;
			console.log(display(rows).toString());
		});
	}

	db.end();
});

function generatePrettyRow(row) {
	return Object.keys(row).map(function (key) {return row[key]});
}

function display(rows) {

//tables not yet implemented sorry!


 // This is a cool barchart. We will use d3}
