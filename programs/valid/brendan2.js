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
		db.query('select Patient.*, Diagnosis.Description from Patient, Diagnosis where (Patient.PatientSerNum = Diagnosis.PatientSerNum) AND (Diagnosis.DiagnosisCode like "C50%" OR Diagnosis.DiagnosisCode like "C16%" OR Diagnosis.DiagnosisCode like "C34%") AND  (Patient.Sex like "f%")', function(err, rows, fields) {
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

	db.end();
});

function generatePrettyRow(row) {
	return Object.keys(row).map(function (key) {return row[key]});
}

function display(rows) {
	var table = new Table({
		head: ["PatientSerNum","OncologistFlag"]
	});
	for (var i_Doctor = 0; i_Doctor  < rows.length; i_Doctor) {
		var Doctor = {
		    PatientSerNum: rows[i_Doctor].PatientSerNum,
		    OncologistFlag: rows[i_Doctor].OncologistFlag,
		}//How do you like me now?
	 console.log(Doctor);		table.push(generatePrettyRow(Doctor));
	}
	return table;
}
