import MySQLdb
from prettytable import PrettyTable
db=MySQLdb.connect(host='127.0.0.1', user='root', passwd='root', db='oncodb')

ontology={"consult_referral_received": {"info":["Task.LastUpdated as TimeStamp","Patient.PatientSerNum","Patient.Sex","Patient.DateOfBirth","Patient.PostalCode","PatientDoctor.DoctorSerNum","PatientDoctor.OncologistFlag","Diagnosis.Description","Task.CreationDate","Task.DueDateTime","Priority.PriorityCode","Task.CompletionDate"],"tables":[	"Patient","Priority","Task","Alias","Diagnosis","PatientDoctor","Doctor"],"constraints": {"Alias.AliasName":"\"CRR with Origin\"","Alias.AliasSerNum":"Task.AliasSerNum","Task.PatientSerNum":"Patient.PatientSerNum","Task.PrioritySerNum":"Priority.PrioritySerNum","Task.Status":"\"Open\"","Diagnosis.PatientSerNum":"Patient.PatientSerNum","PatientDoctor.PatientSerNum":"Patient.PatientSerNum"},"period":"Task"},"initial_consult_booked": {"info":["Task.LastUpdated as TimeStamp","Patient.PatientSerNum","Patient.Sex","Patient.DateOfBirth","Patient.PostalCode","PatientDoctor.DoctorSerNum","PatientDoctor.OncologistFlag","Diagnosis.Description","Task.CreationDate as TimeStamp","Task.DueDateTime","Priority.PriorityCode","Task.CompletionDate"],"tables":["Patient","Priority","Task","Alias","Diagnosis","PatientDoctor","Doctor"],"constraints": {"Alias.AliasName":"\"CRR with Origin\"","Alias.AliasSerNum":"Task.AliasSerNum","Task.PatientSerNum":"Patient.PatientSerNum","Task.PrioritySerNum":"Priority.PrioritySerNum","Task.Status":"\"In Progress\"","Diagnosis.PatientSerNum":"Patient.PatientSerNum","PatientDoctor.PatientSerNum":"Patient.PatientSerNum"},"period":"Task"},"initial_consult_completed": {"info":["Task.LastUpdated as TimeStamp","Patient.PatientSerNum","Patient.Sex","Patient.DateOfBirth","Patient.PostalCode","PatientDoctor.DoctorSerNum","PatientDoctor.OncologistFlag","Diagnosis.Description","Task.CreationDate as TimeStamp","Task.DueDateTime","Priority.PriorityCode","Task.CompletionDate"],"tables":["Patient","Priority","Task","Alias","Diagnosis","PatientDoctor","Doctor"],"constraints": {"Alias.AliasName":"\"CRR with Origin\"","Alias.AliasSerNum":"Task.AliasSerNum","Task.PatientSerNum":"Patient.PatientSerNum","Task.PrioritySerNum":"Priority.PrioritySerNum","Task.Status":"\"Completed\"","Diagnosis.PatientSerNum":"Patient.PatientSerNum","PatientDoctor.PatientSerNum":"Patient.PatientSerNum"},"period":"Task"},"ct_sim_booked": {"info":["Patient.PatientSerNum","PatientDoctor.DoctorSerNum","PatientDoctor.OncologistFlag","Patient.Sex","Patient.DateOfBirth","Patient.PostalCode","Diagnosis.Description","Appointment.LastUpdated as TimeStamp"],"tables":["Patient","Appointment","Alias","Diagnosis","PatientDoctor","Doctor"],"constraints": {"Alias.AliasName":"\"Ct-Sim\"","Alias.AliasSerNum":"Appointment.AliasSerNum","Appointment.PatientSerNum":"Patient.PatientSerNum","Diagnosis.PatientSerNum":"Patient.PatientSerNum","PatientDoctor.PatientSerNum":"Patient.PatientSerNum"},"period":"Appointment"},"ct_sim_completed": {"info":["Patient.PatientSerNum","PatientDoctor.DoctorSerNum","PatientDoctor.OncologistFlag","Patient.Sex","Patient.DateOfBirth","Patient.PostalCode","Diagnosis.Description","Appointment.LastUpdated as TimeStamp","Appointment.ScheduledStartTime","Appointment.ScheduledEndTime"],"tables":["Patient","Appointment","Alias","Diagnosis","PatientDoctor","Doctor"],"constraints": {"Alias.AliasName":"\"Ct-Sim\"","Alias.AliasSerNum":"Appointment.AliasSerNum","Appointment.PatientSerNum":"Patient.PatientSerNum","Diagnosis.PatientSerNum":"Patient.PatientSerNum","PatientDoctor.PatientSerNum":"Patient.PatientSerNum","Appointment.Status":"\"Manually Completed\""},"period":"Appointment"},"treatment_began": {"info":["Patient.PatientSerNum","PatientDoctor.DoctorSerNum","PatientDoctor.OncologistFlag","Patient.Sex","Patient.DateOfBirth","Patient.PostalCode","Diagnosis.Description","Appointment.LastUpdated as TimeStamp","Appointment.ScheduledStartTime","Appointment.ScheduledEndTime"],"tables":["Patient","Appointment","Alias","Diagnosis","PatientDoctor","Doctor"],"constraints": {"Alias.AliasName":"\"First Treatment\"","Alias.AliasSerNum":"Appointment.AliasSerNum","Appointment.PatientSerNum":"Patient.PatientSerNum","Diagnosis.PatientSerNum":"Patient.PatientSerNum","PatientDoctor.PatientSerNum":"Patient.PatientSerNum","Appointment.Status":"\"Open\""},"period":"Appointment"},"patient_arrives": {"info":["Patient.PatientSerNum","PatientDoctor.DoctorSerNum","PatientDoctor.OncologistFlag","Patient.Sex","Patient.DateOfBirth","Patient.PostalCode","Diagnosis.Description","PatientLocation.LastUpdated as TimeStamp","PatientLocation.ArrivalDateTime","Appointment.ScheduledStartTime"],"tables":["Patient","Appointment","Diagnosis","PatientDoctor","PatientLocation","Doctor"],"constraints": {"Appointment.PatientSerNum":"Patient.PatientSerNum","Diagnosis.PatientSerNum":"Patient.PatientSerNum","PatientDoctor.PatientSerNum":"Patient.PatientSerNum","Appointment.AppointmentSerNum":"PatientLocation.AppointmentSerNum"},"period":"PatientLocation"},"patient_scheduled": {"info":["Patient.PatientSerNum","PatientDoctor.DoctorSerNum","PatientDoctor.OncologistFlag","Patient.Sex","Patient.DateOfBirth","Patient.PostalCode","Diagnosis.Description","Appointment.LastUpdated as TimeStamp","Appointment.ScheduledStartTime"],"tables":["Patient","Appointment","Diagnosis","PatientDoctor","Doctor"],"constraints": {"Appointment.PatientSerNum":"Patient.PatientSerNum","Diagnosis.PatientSerNum":"Patient.PatientSerNum","PatientDoctor.PatientSerNum":"Patient.PatientSerNum",},"period":"Appointment"}}
events = ["*"]
filters = "and (Patient.PatientSerNum >= 1 and Patient.PatientSerNum <= 5) "



############################################################
# UTILITY METHODS	 		 							   #
############################################################
def clean(items):
	return list(set(items))

def getResult(actor_type, name):
	return results.get_type(actor_type)[name]

############################################################
# OBJECT CLASSES	 		 							   #
############################################################

class Patient(object):
	def __init__(self, pID, pSex, pBirthyear, pPostalcode):
		self.id = pID
		self.sex = pSex
		self.birthyear = pBirthyear
		self.postalcode = pPostalcode
		self.events = []
		self.diagnosis = []
		self.doctors = []

	def __hash__(self):
		return hash(self.id)

	def __eq__(self, other):
		if other == None: return False
		else: return (self.id) == (other.id)

	def __getitem__(self, field):
		if field == "ID":
			return self.id 
		if field == "SEX":
			return self.sex 
		if field == "BIRTHYEAR":
			return self.birthyear
		if field == "DIAGNOSIS":
			return self.diagnosis
		if field == "POSTALCODE":
			return self.postalcode
		if field == "DOCTORS" or field == "DOCTOR":
			return self.doctors

	def __repr__(self):
		return str(self.id)

	def __str__(self):
		return "Patient (%s): %s born in %s with postalcode %s." % (str(self.id), self.sex, str(self.birthyear), self.postalcode)

	def addDoctor(self, doctor):
		self.doctors += doctor
		self.doctors = clean(self.doctors)

	def addDiagnosis(self, diagnosis):
		self.diagnosis += diagnosis 
		self.diagnosis = clean(self.diagnosis)
	
	def addEvent(self, event):
		self.events += event 
		self.events = clean(self.events)
		self.events.sort(key=lambda x: x.time, reverse=False)

	@staticmethod
	def getfields():
		return ['ID', 'SEX', 'BIRTHYEAR', 'POSTALCODE', 'DIAGNOSIS', "DOCTORS"]

class Doctor(object):
	def __init__(self, pID, pOncologist):
		self.id = pID
		self.oncologist = pOncologist
		self.patients = []
		self.diagnosis = []

	def __getitem__(self, field):
		if field == "ID":
			return self.id
		if field == "ONCOLOGIST":
			return self.oncologist
		if field == "PATIENTS":
			return self.patients
		if field == "DIAGNOSIS":
			return self.diagnosis

	def __hash__(self):
		return hash(self.id)

	def __eq__(self, other):
		if other == None: return False
		return (self.id) == (other.id)

	def __repr__(self):
		return str(self.id)

	def __str__(self):
		return "Doctor (%s): Oncologist: %s" % (str(self.id), str(self.oncologist))

	def addPatient(self, patient):
		self.patients += patient
		self.patients = clean(self.patients)

	def addDiagnosis(self, diagnosis):
		self.diagnosis += diagnosis
		self.diagnosis = clean(self.diagnosis)

	@staticmethod
	def getfields():
		return ['ID', 'ONCOLOGIST', 'PATIENTS', 'DIAGNOSIS']

class Diagnosis(object):
	def __init__(self, pDiagnosis):
		self.diagnosis = pDiagnosis

	def __getitem__(self, field):
		if field == "DIAGNOSIS":
			return self.diagnosis

	def __hash__(self):
		return hash(self.diagnosis)

	def __eq__(self, other):
		if other == None: return False
		return (self.diagnosis == other.diagnosis)

	def __repr__(self):
		return "\"" + str(self.diagnosis) + "\""

	def __str__(self):
		return "Diagnosis: %s" % (self.diagnosis)

	@staticmethod
	def getfields():
		return ['DIAGNOSIS']

class Event(object):
	def __init__(self, pName, pTime, pPatient, pDoctor):
		self.name = pName 
		self.time = pTime 
		self.patient = pPatient
		self.doctor = pDoctor

	def __getitem__(self, field):
		if field == "NAME":
			return self.name
		if field == "TIME":
			return self.time
		if field == "PATIENT":
			return self.patient
		if field == "DOCTOR":
			return self.doctor

	def __hash__(self):
		return hash(self.name + str(self.patient))

	def __eq__(self, other):
		if other == None: return False
		return (self.name == other.name)

	def __str__(self):
		return str(self.name + " " + str(self.patient) + " " + str(self.doctor))

	def __repr__(self):
		return str(self.name + " " + str(self.patient) + " " + str(self.doctor))

	def addDoctor(self, doctor):
		self.doctor += doctor
		self.doctor = clean(self.doctor)

	@staticmethod
	def getfields():
		return ["NAME", "TIME", "PATIENT", "DOCTOR"]

class Results(object):
	"""
		This global results object contains mutliple types of information. 
		It holds a list of Patient objects, and their associated data. 
		It holds a list of Doctor objects and their associated data. 
		It holds a list of Diagnosis objects and their associated data. 
		It holds a list of Event objects and their associated data. 
	"""
	def get_type(self, type):
		if type == "PATIENTS" or type == "PATIENT":
			return self.patients
		if type == "DOCTOR":
			return self.doctors 
		if type == "DIAGNOSIS":
			return self.diagnosis
		if type == "EVENT":
			return self.events

	def add_actor(self, actor, type):
		actors = self.get_type(type)

		if actors.get(actor, None) == None:
			actors[actor] = actor
		else:
			if type == "PATIENTS" or type == "PATIENT":
				actors[actor].addDoctor(actor.doctors)
				actors[actor].addDiagnosis(actor.diagnosis)
				actors[actor].addEvent(actor.events)
			if type == "DOCTOR":
				actors[actor].addPatient(actor.patients)
				actors[actor].addDiagnosis(actor.diagnosis)
			if type == "EVENT":
				actors[actor].addDoctor(actor.doctor)



	def __init__(self, raw_data):
		
		self.patients = {}
		self.doctors = {}
		self.diagnosis = {}
		self.events = {}
		
		def incorporateData(self, item, event):

			dataNotFound =  "Unknown"

			eventName = 	event 
			patId 	= 		item.get('PatientSerNum', dataNotFound)
			sex = 			item.get('Sex', dataNotFound)
			birthyear = 	item.get('DateOfBirth', dataNotFound)
			description = 	item.get('Description', dataNotFound)
			postalcode = 	item.get('PostalCode', dataNotFound)
			docId = 		item.get('DoctorSerNum', dataNotFound)
			oncologist = 	item.get('OncologistFlag')
			time = 			item.get('TimeStamp', dataNotFound)
			diagnosis = 	item.get('Description', dataNotFound)

			# Construct the Patient Object.
			patient = Patient(patId, sex, birthyear, postalcode) 
			
			# Construct the Doctor Object. 
			doctor = Doctor(docId, oncologist)
			
			# Construct the Diagnosis Object.
			diagnosis = Diagnosis(description)

			# Construct the events object 
			event = Event(eventName, time, patId, [doctor])
			
			# Incorporate the data between objects.
			patient.addDoctor([doctor])
			patient.addDiagnosis([diagnosis])
			patient.addEvent([event])
			doctor.addPatient([patient])
			doctor.addDiagnosis([diagnosis])

			# Add the items to our Hash Tables.
			self.add_actor(patient, "PATIENT")
			self.add_actor(doctor, "DOCTOR")
			self.add_actor(diagnosis, "DIAGNOSIS")
			self.add_actor(event, "EVENT")

		# Add the data to our hashes. 
		for event in raw_data:
			for item in raw_data[event]: 
				incorporateData(self, item, event)


############################################################
# FUNCTIONAL METHODS 		 							   #
############################################################

def get_results(events):
	"""
		This method gets the information from the database.
		It takes into account information from ontology.json, as well as filters. 
	"""

	# If our events set is empty, we assume all of them. 
	if events[0] == '*':
		events = ontology.keys()

	# Our Hashmap of results. 
	results = {}

	for event in events:
		
		# The information we want.  
		query = "SELECT "
		for ix in xrange(0, len(ontology[event]['info'])):
			query += ontology[event]['info'][ix]

			if(ix != len(ontology[event]['info']) - 1):
				query += ", " 

		# The tables where that information is located. 
		query += " FROM " 
		for ix in xrange(0, len(ontology[event]['tables'])):
			query += ontology[event]['tables'][ix]

			if(ix != len(ontology[event]['tables']) - 1):
				query += ", "


		# The constraints we have inherently. 
		query += " WHERE "
		for key,val in ontology[event]['constraints'].iteritems():
			query += key + "=" + val + " and "
		query = query[:-4]


		# The constraints from our filters. 
		periodTable = ontology[event]['period']
		query += filters.replace("PERIOD", periodTable)

		# Terminate the query.
		query += ";" 

		# Get the appropriate data. 
		a = db.cursor(MySQLdb.cursors.DictCursor)
		a.execute(query) 

		results[event] = a.fetchall() 

	return results 

############################################################
# COMPUTATION METHODS 		 							   #
############################################################

def tableDecleration(actorType, field, results):
	"""
		Returns a dictionary that counts the actors by their fields.  
	"""
	table = {}
	table["Actor"] = actorType
	table["Field"] = field 
	actor_table = results.get_type(actorType)	

	for key in actor_table:
		value = actor_table[key][field]

		if table.get(str(value), None) == None:
			table[str(value)] = 1
		else:
			table[str(value)] += 1

	return table

def printHeader(name):
	print "-" * len(name)
	print name
	print "-" * len(name)

def printSpace():
	print "\n" *1

def printTable(table):
	"""
		Pretty prints tables. 
	"""
	pretty_table = PrettyTable([table["Field"], "Count"])

	for key in table:
		if key != "Actor" and key != "Field":
			pretty_table.add_row([key, table[key]])

	print pretty_table

def printActor(results, name, actor_type, tabCount = 0):
	"""
		Function handles printing actors, their attributes, and nested actors. 
	"""
	tabCount -= 1; 

	if tabCount == 0:
		printHeader(str(getResult(actor_type, name)))
	else:
		print (("\t") * tabCount) + str(getResult(actor_type, name))
	

def printActorAttributes(results, name, actor_type, attr_list = None, tabCount=0):

	# If we are printing a Patient
	if actor_type == "PATIENT":
		if attr_list == None:
			attr_list = Patient.getfields()		

	# If we are printing a Diagnosis
	if actor_type == "DIAGNOSIS":
		if attr_list == None:
			attr_list = Diagnosis.getfields()
	
	# If we are printing a Doctor.
	if actor_type == "DOCTOR":
		if attr_list == None:
			attr_list = Doctor.getfields()

	print_string = actor_type.title() + ": "

	for attr in attr_list:
		print_string += attr.title() + ": " + str(getResult(actor_type, name)[attr]) + " "

	print print_string


def printPatientTimeline(results, name):
	printHeader("Patient Timeline for: " + str(name))

	patient = results.get_type("PATIENT")[name]

	for event in patient.events:
		print "[{0:<19}]: {1:<25} with doctor(s) {2:<10}".format(str(event.time), str(event.name), str(event.doctor))

	printSpace()

def printLength(table):
	print "Table of {0} counted by {1} has length {2}".format(table["Actor"], table["Field"], len(table) - 2)

def printTableItem(table, item):
 	if item != "Field" and item != "Actor":
		print "{0:<25}: {1:<25}".format(str(item), str(table[item]))

# We want to return a Hashmap of all the information we want.
results = Results(get_results(events))


############################################################
# SEQUENCE UTILITY METHODS	 		 					   #
############################################################

class Sequence(object):
	States = ["Normal", "Or", "Any", "Not", "End", "Terminated"]

	def __init__(self, pSequence = None, pPatientId = None):
		# We keep track of the global sequence in each sequence. 
		self.globSequence = pSequence
		# We take the first event items type. 
		self.currentState = pSequence[0][0]
		# We take the first event items list of events. 
		self.nextEvents   = pSequence[0][1]
		# We initialize this sequence list to empty.
		self.eventList    = []
		# We want to keep track of which sequence item we're on. 
		#  This way we'll know if we have to look at the next item. 
		self.currentItemNumber = 0
		# We need to keep track of the patient this event is for. 
		self.patientId = pPatientId

	def __repr__(self):
		return str(self.eventList)

	def selfCopy(self, event):
		seq = Sequence(self.globSequence, self.patientId)
		seq.currentState = self.currentState
		seq.nextEvents = self.nextEvents[:]
		seq.eventList = self.eventList[:]
		seq.currentItemNumber = self.currentItemNumber
		seq.currentState = self.currentState 
		return seq

	def add_item(self, event):

		def validateEvent(state, name, id):
			return self.currentState == state and event.name in self.nextEvents and event.patient == self.patientId

		def validateFutureEvent(state, name, id):
			if self.currentState == state:
				nextItem = self.currentItemNumber + 1
				nextState = self.globSequence[nextItem][0]
				nextEvents = self.globSequence[nextItem][1]
				if name in nextEvents: return True
			else:
				return False

		def incorporateAndAdvance(event, double = False):
			if double == False:
				self.currentItemNumber += 1
				self.eventList += [event]
				self.currentState = self.globSequence[self.currentItemNumber][0]
				self.nextEvents =  self.globSequence[self.currentItemNumber][1]
			else:
				self.currentItemNumber += 2
				self.eventList += [event]
				self.currentState = self.globSequence[self.currentItemNumber][0]
				self.nextEvents =  self.globSequence[self.currentItemNumber][1]

		def incorporateAndStay(event):
			self.eventList += [event]

		def terminateSequence():
			self.currentState = "Terminated"


		# Simplest form, just see if the event name matches. 
		if validateEvent("Normal", event.name, event.patient):
			incorporateAndAdvance(event)
		
		# If it's an or we want to take it and go. 
		if validateEvent("Or", event.name, event.patient):
			oldSeq = self.selfCopy(event)
			incorporateAndAdvance(event)
			return oldSeq

		# If it's an in any order, we need to be more careful. 
		# Either the event could be in our list, or it could be in the future.
		if validateEvent("Any", event.name, event.patient):
			oldSeq = self.selfCopy(event)
			incorporateAndStay(event)
			return oldSeq
		if validateFutureEvent("Any", event.name, event.patient):
			oldSeq = self.selfCopy(event)
			incorporateAndAdvance(event, double = True)
			return oldSeq

		# If it's a not item, we do something similar to Any. 
		#  If the item occurrs, then we need to terminate our sequence. 
		#  Else, we need to check if the current event is next in line somewhere. 
		if validateEvent("Not", event.name, event.patient):
			terminateSequence()
		if validateFutureEvent("Not", event.name, event.patient):
			incorporateAndAdvance(event, double = True)

class SequenceList(object):
	def __init__(self, pSequence, pPatientId):
		self.sequences = []
		self.sequence  = pSequence
		self.patient   = pPatientId

		self.sequences += [Sequence(pSequence, pPatientId)]

	def add_item(self, event):
	
		# Confirm the validity of the patient. 		
		if event.patient != self.patient: return

		# For each sequence in our list, we want to add the event if applicable. 
		# If it's a normal event, we simply add it if it's the next event we want. 
		# If it's an OR event, we add it if it's in the set of events we want.
		# 	We then create a duplicate less the event we just added. 
		# If it's an AND event, we add it if it's in the set of events we want. 
		# 	We then create a duplicate less the event we just added. 
		new_branches = []	# The new branches must be added at the end. 

		for seq in self.sequences:
			if seq.currentState == "Normal":
				seq.add_item(event)
			else:
				oldSeq = seq.add_item(event)
				if oldSeq != None:
					new_branches += [oldSeq]

		# We finall add all our new branches. 
		self.sequences += new_branches

def get_events_as_list():
	event_list = []
	for event in results.get_type("EVENT"):
		event_list += [results.get_type("EVENT")[event]]
	return event_list

def findSequences(sequence):
	# These are all the events in our Program.
	# We want them sorted by time. 
	global_event_list = get_events_as_list()
	global_event_list.sort(key=lambda r: r.time)

	# We want a hashmap from Patient ID to Sequence Objects. 
	patSeq = {}

	# Here we do the actual work. 
	for event in global_event_list:

		patientId = event.patient

		# Our patient does not have sequences associated with them yet.
		if patSeq.get(patientId, None) == None:
			patSeq[patientId] = SequenceList(sequence, patientId)
	
		# Now we simple add the event. 
		patSeq[patientId].add_item(event)

	# This is what will contain the clean and returnable sequence lists.
	sequenceLists = {}

	for key, val in patSeq.iteritems():
		sequenceLists[key] = []

		for v in val.sequences:
			if v.currentState == "End":
				sequenceLists[key] += [v]

			if v.currentState == "Any" and (v.currentItemNumber == (len(sequence) -2)):
				sequenceLists[key] += [v]

	return sequenceLists

def printSequenceItem(sequenceList):

	if sequenceList == []: return 

	for sequence in sequenceList:
		sequenceString = "{0:<5}: ".format(str(sequence.patientId))

		for event in sequence.eventList:
			date = str(event.time.strftime('%b %d %Y'))
			patient = str(event.patient)
			doctors = str(event.doctor)
			name    = str(event.name)

			sequenceString += ("{0:<10} {1:17} => ".format(date, name))

		sequenceString += "END"
		print sequenceString

############################################################
# GENERATED COMPUTATIONS	 		 					   #
############################################################
#
#
#
a = tableDecleration( "PATIENT", "SEX", results)
for i in a: 
	printTableItem(a,i)
	
