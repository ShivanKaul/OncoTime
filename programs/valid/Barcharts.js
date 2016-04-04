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
		db.query('select * from Patient where  (Patient.Sex like "m%" OR  Patient.Sex like "f%") AND  (Patient.DateOfBirth > 1950 AND Patient.DateOfBirth < 1970) AND  (Patient.PatientSerNum > 1 AND Patient.PatientSerNum < 250 OR Patient.PatientSerNum > 300 AND Patient.PatientSerNum < 400 OR Patient.PatientSerNum = 1001)', function(err, rows, fields) {
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



	 console.log('Table hasn't been implemented yet, sorry!');
	 console.log('Table hasn't been implemented yet, sorry!');
	 console.log('Table hasn't been implemented yet, sorry!');}
