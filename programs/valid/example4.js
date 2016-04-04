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
		db.query('select * from Patient where  (Sex like "m%" OR  Sex like "f%") AND  (DateOfBirth > 1950 AND DateOfBirth < 1970 AND ) AND  (DoctorSerNum > 1 AND DoctorSerNum < 250 AND DoctorSerNum > 300 AND DoctorSerNum < 400 AND DoctorSerNum = 1001)', function(err, rows, fields) {
			if (err) throw err;
			console.log(display(rows));
		});
	}

	db.end();
});

function display(rows) {


}
