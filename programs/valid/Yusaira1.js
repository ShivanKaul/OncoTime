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
		db.query('select * from Patient where  (Patient.DateOfBirth = 1971 OR Patient.DateOfBirth = 1972) AND  (Patient.Sex like "m%" OR  Patient.Sex like "f%")', function(err, rows, fields) {
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

//tables not yet implemented sorry!
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
	 console.log(sex: rows[i_Sex].Sex, postalcode: rows[i_PostalCode].PostalCode);
	var table = new Table({
		head: ["PatientSerNum","OncologistFlag"]
	});
	for (var i_Doctor = 0; i_Doctor  < rows.length; i_Doctor) {
		var Doctor = {
		    PatientSerNum: rows[i_Doctor].PatientSerNum,
		    OncologistFlag: rows[i_Doctor].OncologistFlag,
		}//How do you like me now?
	var table = new Table({
		head: ["Description"]
	});
	for (var i_Diagnosis = 0; i_Diagnosis  < rows.length; i_Diagnosis) {
		var Diagnosis = {
		    Description: rows[i_Diagnosis].Description,
		}//How do you like me now?
		table.push(generatePrettyRow(Diagnosis));
	}
	return table;
		table.push(generatePrettyRow(Doctor));
	}
	return table;
		table.push(generatePrettyRow(Patient));
	}
	return table;


}
