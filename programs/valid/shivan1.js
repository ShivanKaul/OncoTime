<<<<<<< 510ddaa7969719b7b93d018f7b1849b2ecbafc4a
var mysql = require('mysql');
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
		db.query('select * from Patient where diagnosis = 'prostate' AND birthyear > 1990 AND birthyear < 2015', function(err, rows, fields) {
			if (err) throw err;
			console.log(display(rows));
		});
	}

	db.end();
});

function display(rows) {
	for (var i = 0; i < rows.length; i++) {
		var Patient = {
		    id: rows[i].PatientSerNum,
		    dob: rows[i].DateOfBirth,
		    sex: rows[i].Sex,
		    postalcode: rows[i].PostalCode
		}
		process.stdout.write('Patient : ');
		console.log(Patient);
	}

}
=======
>>>>>>> fixed pretty printing bugs
