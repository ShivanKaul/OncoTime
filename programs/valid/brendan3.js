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
		db.query('select Patient.*, Diagnosis.Description from Patient, Diagnosis where (Patient.PatientSerNum = Diagnosis.PatientSerNum) AND (Diagnosis.DiagnosisCode like "C61%") AND  (Patient.DateOfBirth > 1986 AND Patient.DateOfBirth < 1996) AND  (Patient.Diagnosis like "prostate%") AND  (Patient.Sex like "m%")', function(err, rows, fields) {
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

}
