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
		db.query('select * from Patient where birthyear > 1986 AND birthyear < 1996 AND diagnosis = 'prostate' AND sex = 'm' AND sex = 'f'', function(err, rows, fields) {
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
