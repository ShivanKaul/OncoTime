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
		db.query('select * from Patient where  (DateOfBirth = 1951 OR DateOfBirth = 1952 OR DateOfBirth = 1953 OR DateOfBirth = 1954 OR DateOfBirth = 1955) AND  (Sex like "m%")', function(err, rows, fields) {
			if (err) throw err;
			console.log(display(rows));
		});
	}

	db.end();
});

function display(rows) {






}
