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
		db.query('select Patient.*, Diagnosis.Description from Patient, Diagnosis where (Patient.PatientSerNum = Diagnosis.PatientSerNum) AND (Diagnosis.DiagnosisCode like "C71%") AND  (Patient.Diagnosis like "breast%") AND  (Patient.PostalCode like "k2g6k8%") AND  (Patient.DateOfBirth = 1967) AND  (Patient.Sex like "m%") AND  (Patient.PatientSerNum = 1000 OR Patient.PatientSerNum = 2000 OR Patient.PatientSerNum = 3000)', function(err, rows, fields) {
			if (err) throw err;
			console.log(display(rows).toString());
		});
	}

		db.query('/*doctors filtering hasn't been implemented yet, sorry! */', function(err, rows, fields) {
			if (err) throw err;
			console.log(display(rows).toString());
		});
	}

		db.query('/*period filtering hasn't been implemented yet, sorry! */', function(err, rows, fields) {
			if (err) throw err;
			console.log(display(rows).toString());
		});
	}

		db.query('/*events filtering hasn't been implemented yet, sorry! */', function(err, rows, fields) {
			if (err) throw err;
			console.log(display(rows).toString());
		});
	}

		db.query('select Patient.*, Diagnosis.Description from Patient, Diagnosis where (Patient.PatientSerNum = Diagnosis.PatientSerNum) AND (Diagnosis.DiagnosisCode like "C71%") AND  (Patient.Diagnosis like "brain%") AND  (Patient.Sex like "f%")', function(err, rows, fields) {
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
		head: ["DateOfBirth","Diagnosis","Sex","PatientSerNum","PostalCode"]
	});
	for (var i_Patient = 0; i_Patient  < rows.length; i_Patient) {
		var Patient = {
		    DateOfBirth: rows[i_Patient].DateOfBirth,
		    Diagnosis: rows[i_Patient].Description,
		    Sex: rows[i_Patient].Sex,
		    PatientSerNum: rows[i_Patient].PatientSerNum,
		    PostalCode: rows[i_Patient].PostalCode,
		}//How do you like me now?
	 console.log(Patient);		table.push(generatePrettyRow(Patient));
	}
	return table;
}
