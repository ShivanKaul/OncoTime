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
		db.query('select * from Patient,Diagnosis limit 10', function(err, rows, fields) {
    if (err) throw err;
    patient_fns = [
    function TEST(rows){
            
            diagnosis_fns = [
                function PrintVar(p){console.log("ho")},
                function PrintVar(d){console.log("hey")}
                ]
                for(j =0; j < diagnosis_fns.length; j++){ 
                        diagnosis_fns[j](rows);  	 
                        console.log(j);
                        console.log("working")
                        } 
            },

        function (){console.log("ha")}
    ]//swap this with the thing below it
    console.log("about to work")
    console.log(rows.length)
    foreach_fname(rows,patient_fns);

    console.log("done working")

    });
    }
            db.end();
    
});

function print_var(row) {
 	 console.log(row)
}


function table_display(row){}
function barchart_display(row){}
function foreach_fname(rows, fns){ 
 for(i =0; i < rows.length; i++){
     console.log(fns[0].name);
 	 for(j =0; j < fns.length; j++){ 
 		 fns[j](rows[i]);  	 
        } 
  } 
}

