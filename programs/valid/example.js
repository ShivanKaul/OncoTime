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
		db.query('select * from Patient where  (Patient.PatientSerNum = 1 OR Patient.PatientSerNum = 2 OR Patient.PatientSerNum > 1200 AND Patient.PatientSerNum < 123123) AND  (Patient.DateOfBirth = 1951 OR Patient.DateOfBirth = 1952 OR Patient.DateOfBirth = 1953 OR Patient.DateOfBirth = 1954 OR Patient.DateOfBirth = 1955) AND  (Patient.Sex like "m%")', function(err, rows, fields) {
			if (err) throw err;
			console.log(display(rows).toString());
		});
	}

		db.query('/*patient filtering hasn't been implemented yet, sorry! */', function(err, rows, fields) {
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

	var table = new Table({
		head: ["DateOfBirth","Sex","PatientSerNum","PostalCode"]
	});
	for (var i_Patient = 0; i_Patient  < rows.length; i_Patient) {
		var Patient = {
		    DateOfBirth: rows[i_Patient].DateOfBirth,
		    Sex: rows[i_Patient].Sex,
		    PatientSerNum: rows[i_Patient].PatientSerNum,
		    PostalCode: rows[i_Patient].PostalCode,
		}//How do you like me now?
	 console.log(Patient);		table.push(generatePrettyRow(Patient));
	}
	return table;

	 console.log('Table hasn't been implemented yet, sorry!');}
