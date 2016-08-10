var animate_subjects = 	['pigeon','cat','cheetah','dog','dolphin','camel','gorilla',
						'shark','bear','kangaroo','butterfly','turtle','panda','peacock',
						'pig','squirrel','owl','lion']

var inanimate_subjects = 	['crayon','toilet','kite','piano','football','telephone','computer',
							'car','marble','light bulb','towel','boat','escalator',
							'ATM','camera','microwave','hose','frying pan']

var subject_verb_examples =	['mouse', 'zebra', 'bus', 'sword','shirt','elephant']

var number = ['singular', 'plural']

var definiteness = ['definite', 'indefinite']

var tense = ['present','past','progressive']

var contexts = ['bare','generic','specific']

var male_names = ['Michael','James','John','Robert','David']

var female_names = ['Mary','Jennifer','Patricia','Linda','Elizabeth']

var name_1 = ''

var name_2 = ''

var example_features =	[	{subject:'mouse',number:'singular',definiteness:'indefinite',animacy:'animate',
							tense:'progressive',context:'specific',sentence:'A mouse is being chased by a cat.'},
							{subject:'elephant',number:'plural',definiteness:'indefinite',animacy:'animate',
							tense:'present',context:'bare',sentence:'Elephants use their trunks to eat.'},
							{subject:'shirt',number:'singular',definiteness:'definite',animacy:'inanimate',
							tense:'past',context:'bare',sentence:'The shirt ripped.'},
							{subject:'bus',number:'plural',definiteness:'definite',animacy:'inanimate',
							tense:'past',context:'specific',sentence:'The buses blocked traffic.'},
							{subject:'sword',number:'plural',definiteness:'indefinite',animacy:'inanimate',
							tense:'present',context:'generic',sentence:'Swords are sharp.'},
							{subject:'zebra',number:'singular',definiteness:'definite',animacy:'animate',
							tense:'present',context:'generic',sentence:'Zebras have stripes.'}]

var features = []

function showSlide(id) {
	$(".slide").hide();
	$("#"+id).show();
}

function shuffle(v) { // non-destructive.
    newarray = v.slice(0);
    for(var j, x, i = newarray.length; i; j = parseInt(Math.random() * i), x = newarray[--i], newarray[i] = newarray[j], newarray[j] = x);
    return newarray;
};

function random(a,b) {
    if (typeof b == "undefined") {
	a = a || 2;
	return Math.floor(Math.random()*a);
    } else {
	return Math.floor(Math.random()*(b-a+1)) + a;
    }
}

function pluralize(subject){
	if (subject == 'bus') {
		return subject.concat('es')
	}
	else if (subject == 'butterfly'){
		return 'butterflies';
	}
	else if (subject == 'mouse'){
		return 'mice';
	}
	else {
		return subject.concat('s')
	}
}

function capitalize(subject){
	var new_subject = subject.charAt(0).toUpperCase();
	new_subject = new_subject.concat(subject.substring(1, subject.length));
	return new_subject;
}

function make_explanation(subject, rating){
	var explanation;
	if (subject == 'mouse' && rating == 0){
		explanation = "Good job. This sentence is about a specific mouse that is being chased. Click the button below one more time to continue."
	}
	else if (subject == 'mouse' && rating == 1){
		explanation = "You should have said that this sentence was about a specific mouse. It is about a mouse that is being chased, so it is about a specific mouse. Click the button below one more time to continue."
	}
	else if (subject == 'bus' && rating == 0){
		explanation = "Good job. This sentence is about some set of buses that blocked traffic, so it is about a specific group of buses. Click the button below one more time to continue."
	}
	else if (subject == 'bus' && rating == 1){
		explanation = "You should have said that this sentence was about a specific group of buses. It is about some set of buses that blocked traffic, so it is about a specific group of buses. Click the button below one more time to continue."
	}
	else if (subject == 'sword' && rating == 0){
		explanation = "You should have said that this sentence was about swords in general. It expresses a generalization about swords, rather than saying anything about any specific swords. Click the button below one more time to continue."
	}
	else if (subject == 'sword' && rating == 1){
		explanation = "Good job. This sentence expresses a generalization about swords, rather than saying anything about any specific swords. Click the button below one more time to continue."
	}
	else if (subject == 'zebra' && rating == 0){
		explanation = "You should have said that this sentence was about zebras in general. It expresses a general fact about zebras, rather than any specific group of zebras. Click the button below one more time to continue." 
	}
	else if (subject == 'zebra' && rating == 1){
		explanation = "Good job. This sentence expresses a fact about zebras in general, rather than any specific group of zebras. Click the button below one more time to continue."
	}
	else if (subject == 'elephant' && rating == 0){
		explanation = "You should have said that this sentence was about elephants in general. It expresses a generalization about elephants, rather than any specific group of elephants. Click the button below one more time to continue." 
	}
	else if (subject == 'elephant' && rating == 1){
		explanation = "Good job. This sentence expresses a fact about elephants in general, rather than any specific group of elephants. Click the button below one more time to continue."
	}
	else if (subject == 'shirt' && rating == 0){
		explanation = "Good job. This sentence is about a specific shirt that was ripped. Click the button below one more time to continue." 
	}
	else if (subject == 'shirt' && rating == 1){
		explanation = "You should have said that this sentence was about a specific shirt. It is about a particular shirt that was ripped, so it is about a specific mouse. Click the button below one more time to continue."
	}
	return explanation;
}

function make_names(){
	male = shuffle(male_names)[0]
	female = shuffle(female_names)[0]
	if (random(0,1) == 0){
		name_1 = male
		name_2 = female
	}
	else {
		name_1 = female
		name_2 = male
	}
}

function make_subject(subject, number, definiteness){
	var new_subject;
	if (definiteness == 'definite'){
		new_subject = 'The ';
		if (number == 'singular'){
			new_subject = new_subject.concat(subject);
		}
		else {
			new_subject = new_subject.concat(pluralize(subject));
		}
	}
	else {
		if (number == 'singular'){
			if (['a','e','i','o','u'].indexOf(subject.toLowerCase().charAt(0)) >= 0){
				new_subject = 'An ';
			}
			else{
				new_subject = 'A ';
			}
			new_subject = new_subject.concat(subject);
		}
		else {
			new_subject = pluralize(subject);
			new_subject = capitalize(new_subject);
		}
	}
	return new_subject
}

function make_sentence_order(){
	sentence_order = []
	example_contexts  = []
	shuffled_examples = shuffle(example_features)
	for (i = 0; i < shuffled_examples.length; i++){
		if (example_contexts.indexOf(shuffled_examples[i]['context']) == -1){
			sentence_order.push(shuffled_examples[i])
			example_contexts.push(shuffled_examples[i]['context'])
		}
	}
	last_context = sentence_order[sentence_order.length - 1]['context']
	subjects = shuffle(animate_subjects.concat(inanimate_subjects))
	target_features = []
	index = 0
	for (i = 0; i < number.length; i++){
		for (j = 0; j < definiteness.length; j++){
			for (k = 0; k < tense.length; k++){
				for (l = 0; l < contexts.length; l++){
					var anim;
					if (animate_subjects.indexOf(subjects[index]) >= 0){
						anim = 'animate'
					}
					else {
						anim = 'inanimate'
					}
					
					feature_obj = 	{subject:subjects[index],number:number[i],definiteness:definiteness[j],animacy:anim,
									tense:tense[k], context:contexts[l]}
					target_features.push(feature_obj)
					
					index++;
				}
			}
		}
	}
	used_contexts = []
	used_subjects = []
	shuffled_targets = shuffle(target_features);
	for (i = 0; i < shuffled_targets.length; i++){
		shuffled_contexts = shuffle(contexts)
		for (j = 0; j < contexts.length; j++){
			if (shuffled_contexts[j] != last_context && used_contexts.indexOf(shuffled_contexts[j]) == -1){
				for (k = 0; k < shuffled_targets.length; k++){
					if (shuffled_targets[k]['context'] == shuffled_contexts[j] && used_subjects.indexOf(shuffled_targets[k]['subject']) == -1) {
						sentence_order.push(shuffled_targets[k]);
						used_contexts.push(shuffled_contexts[j])
						used_subjects.push(shuffled_targets[k]['subject'])
						last_context = shuffled_contexts[j]
						break
					}
				}
				break
			}
		}
		
		if ((i + 1) % 3 == 0){
			used_contexts = []
		}
		
	}
	return sentence_order
};

function make_qud(subject, qud){
	if (qud == 'generic'){
		return 'Tell me about '.concat(pluralize(subject)).concat('.')
	}
	else if (qud == 'specific'){
		return "Tell me what's happening."
	}
	else {
		return null
	}
}

function make_response(subject,number,definiteness,tense){
	subject_np = make_subject(subject,number,definiteness)
	vp = ''
	if (subject == 'pigeon'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' bobs its head.'
			}
			else if (tense =='past') {
				vp = ' bobbed its head.'
			}
			else {
				vp = ' is bobbing its head.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' bob their heads.'
			}
			else if (tense =='past') {
				vp = ' bobbed their heads.'
			}
			else {
				vp = ' are bobbing their heads.'
			}
		}
	}
	else if (subject == 'cat'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' sleeps a lot.'
			}
			else if (tense =='past') {
				vp = ' slept a lot.'
			}
			else {
				vp = ' is sleeping a lot.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' sleep a lot.'
			}
			else if (tense =='past') {
				vp = ' slept a lot.'
			}
			else {
				vp = ' are sleeping a lot.'
			}
		}
	}
	else if (subject == 'cheetah'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' runs fast.'
			}
			else if (tense =='past') {
				vp = ' ran fast.'
			}
			else {
				vp = ' is running fast.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' run fast.'
			}
			else if (tense =='past') {
				vp = ' ran fast.'
			}
			else {
				vp = ' are running fast.'
			}
		}
	}
	else if (subject == 'dog'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' gnaws on bones.'
			}
			else if (tense =='past') {
				vp = ' gnawed on bones.'
			}
			else {
				vp = ' is gnawing on bones.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' gnaw on bones.'
			}
			else if (tense =='past') {
				vp = ' gnawed on bones.'
			}
			else {
				vp = ' are gnawing on bones.'
			}
		}
	}
	else if (subject == 'dolphin'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' swims in a pod.'
			}
			else if (tense =='past') {
				vp = ' swam in a pod.'
			}
			else {
				vp = ' is swimming in a pod.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' swim in a pod.'
			}
			else if (tense =='past') {
				vp = ' swam in a pod.'
			}
			else {
				vp = ' are swimming in a pod.'
			}
		}
	}
	else if (subject == 'camel'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' carries supplies through the desert.'
			}
			else if (tense =='past') {
				vp = ' carried supplies through the desert.'
			}
			else {
				vp = ' is carrying supplies through the desert.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' carry supplies through the desert.'
			}
			else if (tense =='past') {
				vp = ' carried supplies through the desert.'
			}
			else {
				vp = ' are carrying supplies through the desert.'
			}
		}
	}
	else if (subject == 'gorilla'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' pounds its chest.'
			}
			else if (tense =='past') {
				vp = ' pounded its chest.'
			}
			else {
				vp = ' is pounding its chest.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' pound their chests.'
			}
			else if (tense =='past') {
				vp = ' pounded their chests.'
			}
			else {
				vp = ' are pounding their chests.'
			}
		}
	}
	else if (subject == 'shark'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' attacks swimmers.'
			}
			else if (tense =='past') {
				vp = ' attacked swimmers.'
			}
			else {
				vp = ' is attacking swimmers.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' attack swimmers.'
			}
			else if (tense =='past') {
				vp = ' attacked swimmers.'
			}
			else {
				vp = ' are attacking swimmers.'
			}
		}
	}
	else if (subject == 'bear'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' climbs trees.'
			}
			else if (tense =='past') {
				vp = ' climbed trees.'
			}
			else {
				vp = ' is climbing trees.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' climb trees.'
			}
			else if (tense =='past') {
				vp = ' climbed trees.'
			}
			else {
				vp = ' are climbing trees.'
			}
		}
	}
	else if (subject == 'kangaroo'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' carries its baby in a pouch.'
			}
			else if (tense =='past') {
				vp = ' carried its baby in a pouch.'
			}
			else {
				vp = ' is carrying its baby in a pouch.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' carry their babies in pouches.'
			}
			else if (tense =='past') {
				vp = ' carried their babies in pouches.'
			}
			else {
				vp = ' are carrying their babies in pouches.'
			}
		}
	}
	else if (subject == 'butterfly'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' flaps its wings.'
			}
			else if (tense =='past') {
				vp = ' flapped its wings.'
			}
			else {
				vp = ' is flapping its wings.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' flap their wings.'
			}
			else if (tense =='past') {
				vp = ' flapped their wings.'
			}
			else {
				vp = ' are flapping their wings.'
			}
		}
	}
	else if (subject == 'pig'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' rolls in the mud.'
			}
			else if (tense =='past') {
				vp = ' rolled in the mud.'
			}
			else {
				vp = ' is rolling in the mud.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' roll in the mud.'
			}
			else if (tense =='past') {
				vp = ' rolled in the mud.'
			}
			else {
				vp = ' are rolling in the mud.'
			}
		}
	}
	else if (subject == 'turtle'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' moves slowly.'
			}
			else if (tense =='past') {
				vp = ' moved slowly.'
			}
			else {
				vp = ' is moving slowly.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' move slowly.'
			}
			else if (tense =='past') {
				vp = ' moved slowly.'
			}
			else {
				vp = ' are moving slowly.'
			}
		}
	}
	else if (subject == 'panda'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' eats bamboo.'
			}
			else if (tense =='past') {
				vp = ' ate bamboo.'
			}
			else {
				vp = ' is eating bamboo.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' eat bamboo.'
			}
			else if (tense =='past') {
				vp = ' ate bamboo.'
			}
			else {
				vp = ' are eating bamboo.'
			}
		}
	}
	else if (subject == 'peacock'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' displays its feathers.'
			}
			else if (tense =='past') {
				vp = ' displayed its feathers.'
			}
			else {
				vp = ' is displaying its feathers.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' display their feathers.'
			}
			else if (tense =='past') {
				vp = ' displayed their feathers.'
			}
			else {
				vp = ' are displaying their feathers.'
			}
		}
	}
	else if (subject == 'squirrel'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' buries nuts.'
			}
			else if (tense =='past') {
				vp = ' buried nuts.'
			}
			else {
				vp = ' is burying nuts.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' bury nuts.'
			}
			else if (tense =='past') {
				vp = ' buried nuts.'
			}
			else {
				vp = ' are burying nuts.'
			}
		}
	}
	else if (subject == 'owl'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' hoots.'
			}
			else if (tense =='past') {
				vp = ' hooted.'
			}
			else {
				vp = ' is hooting.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' hoot.'
			}
			else if (tense =='past') {
				vp = ' hooted.'
			}
			else {
				vp = ' are hooting.'
			}
		}
	}
	else if (subject == 'lion'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' chases antelope.'
			}
			else if (tense =='past') {
				vp = ' chased antelope.'
			}
			else {
				vp = ' is chasing antelope.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' chase antelope.'
			}
			else if (tense =='past') {
				vp = ' chased antelope.'
			}
			else {
				vp = ' are chasing antelope.'
			}
		}
	}
	else if (subject == 'crayon'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' melts in the sun.'
			}
			else if (tense =='past') {
				vp = ' melted in the sun.'
			}
			else {
				vp = ' is melting in the sun.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' melt in the sun.'
			}
			else if (tense =='past') {
				vp = ' melted in the sun.'
			}
			else {
				vp = ' are melting in the sun.'
			}
		}
	}
	else if (subject == 'toilet'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' flushes.'
			}
			else if (tense =='past') {
				vp = ' flushed.'
			}
			else {
				vp = ' is flushing.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' flush.'
			}
			else if (tense =='past') {
				vp = ' flushed.'
			}
			else {
				vp = ' are flushing.'
			}
		}
	}
	else if (subject == 'kite'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' floats in the breeze.'
			}
			else if (tense =='past') {
				vp = ' floated in the breeze.'
			}
			else {
				vp = ' is floating in the breeze.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' float in the breeze.'
			}
			else if (tense =='past') {
				vp = ' floated in the breeze.'
			}
			else {
				vp = ' are floating in the breeze.'
			}
		}
	}
	else if (subject == 'piano'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' plays music.'
			}
			else if (tense =='past') {
				vp = ' played music.'
			}
			else {
				vp = ' is playing music.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' play music.'
			}
			else if (tense =='past') {
				vp = ' played music.'
			}
			else {
				vp = ' are playing music.'
			}
		}
	}
	else if (subject == 'football'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' spirals in the air.'
			}
			else if (tense =='past') {
				vp = ' spiraled in the air.'
			}
			else {
				vp = ' is spiraling in the air.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' spiral in the air.'
			}
			else if (tense =='past') {
				vp = ' spiraled in the air.'
			}
			else {
				vp = ' are spiraling in the air.'
			}
		}
	}
	else if (subject == 'telephone'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' rings.'
			}
			else if (tense =='past') {
				vp = ' rang.'
			}
			else {
				vp = ' is ringing.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' ring.'
			}
			else if (tense =='past') {
				vp = ' rang.'
			}
			else {
				vp = ' are ringing.'
			}
		}
	}
	else if (subject == 'computer'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' connects to the internet.'
			}
			else if (tense =='past') {
				vp = ' connected to the internet.'
			}
			else {
				vp = ' is connecting to the internet.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' connect to the internet.'
			}
			else if (tense =='past') {
				vp = ' connected to the internet.'
			}
			else {
				vp = ' are connecting to the internet.'
			}
		}
	}
	else if (subject == 'car'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' travels on the highway.'
			}
			else if (tense =='past') {
				vp = ' traveled on the highway.'
			}
			else {
				vp = ' is traveling on the highway.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' travel on the highway.'
			}
			else if (tense =='past') {
				vp = ' traveled on the highway.'
			}
			else {
				vp = ' are traveling on the highway.'
			}
		}
	}
	else if (subject == 'marble'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' rolls on the floor.'
			}
			else if (tense =='past') {
				vp = ' rolled on the floor.'
			}
			else {
				vp = ' is rolling on the floor.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' roll on the floor.'
			}
			else if (tense =='past') {
				vp = ' rolled on the floor.'
			}
			else {
				vp = ' are rolling on the floor.'
			}
		}
	}
	else if (subject == 'light bulb'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' burns out.'
			}
			else if (tense =='past') {
				vp = ' burned out.'
			}
			else {
				vp = ' is burning out.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' burn out.'
			}
			else if (tense =='past') {
				vp = ' burned out.'
			}
			else {
				vp = ' are burning out.'
			}
		}
	}
	else if (subject == 'towel'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' soaks up water.'
			}
			else if (tense =='past') {
				vp = ' soaked up water.'
			}
			else {
				vp = ' is soaking up water.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' soak up water.'
			}
			else if (tense =='past') {
				vp = ' soaked up water.'
			}
			else {
				vp = ' are soaking up water.'
			}
		}
	}
	else if (subject == 'boat'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' sails across the sea.'
			}
			else if (tense =='past') {
				vp = ' sailed across the sea.'
			}
			else {
				vp = ' is sailing across the sea.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' sail across the sea.'
			}
			else if (tense =='past') {
				vp = ' sailed across the sea.'
			}
			else {
				vp = ' are sailing across the sea.'
			}
		}
	}
	else if (subject == 'escalator'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' carries people upstairs.'
			}
			else if (tense =='past') {
				vp = ' carried people upstairs.'
			}
			else {
				vp = ' is carrying people upstairs.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' carry people upstairs.'
			}
			else if (tense =='past') {
				vp = ' carried people upstairs.'
			}
			else {
				vp = ' are carrying people upstairs.'
			}
		}
	}
	else if (subject == 'ATM'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' dispenses money.'
			}
			else if (tense =='past') {
				vp = ' dispensed money.'
			}
			else {
				vp = ' is dispensing money.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' dispense money.'
			}
			else if (tense =='past') {
				vp = ' dispensed money.'
			}
			else {
				vp = ' are dispensing money.'
			}
		}
	}
	else if (subject == 'camera'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' records video.'
			}
			else if (tense =='past') {
				vp = ' recorded video.'
			}
			else {
				vp = ' is recording video.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' record video.'
			}
			else if (tense =='past') {
				vp = ' recorded video.'
			}
			else {
				vp = ' are recording video.'
			}
		}
	}
	else if (subject == 'microwave'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' heats up food.'
			}
			else if (tense =='past') {
				vp = ' heated up food.'
			}
			else {
				vp = ' is heating up food.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' heat up food.'
			}
			else if (tense =='past') {
				vp = ' heated up food.'
			}
			else {
				vp = ' are heating up food.'
			}
		}
	}
	else if (subject == 'hose'){
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' sprays water.'
			}
			else if (tense =='past') {
				vp = ' sprayed water.'
			}
			else {
				vp = ' is spraying water.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' spray water.'
			}
			else if (tense =='past') {
				vp = ' sprayed water.'
			}
			else {
				vp = ' are spraying water.'
			}
		}
	}
	else {
		if (number == 'singular'){
			if (tense == 'present'){
				vp = ' sizzles.'
			}
			else if (tense =='past') {
				vp = ' sizzled.'
			}
			else {
				vp = ' is sizzling.'
			}
		}
		else {
			if (tense == 'present'){
				vp = ' sizzle.'
			}
			else if (tense =='past') {
				vp = ' sizzled.'
			}
			else {
				vp = ' are sizzling.'
			}
		}
	}
	return subject_np.concat(vp)
}

var numberOfQuestions = 0;

$(document).ready(function() {
	make_names();
	features = make_sentence_order()
    showSlide("instructions");
    $("#instructions #mustaccept").hide();
});

var experiment = {
    data: {},
    intro: function () {
    	if (turk.previewMode) {
	    	$("#instructions #mustaccept").show();
		} else {
			make_names();
			$(".name1").html(name_1);
			$(".name2").html(name_2);
	    	showSlide("intro");
	    	numberOfQuestions = features.length;
	    }
    },
    next: function(num) { 
    	if (num == numberOfQuestions + 1) {
   // or "if (stimuli.length == 0) {...}", etc: test whether it's time to end the experiment
		    showSlide("language");
		    $("#lgerror").hide();
		    $("#lgbox").keypress(function(e){ // capture return so that it doesn't restart experiment
		    	if (e.which == 13) {
		    		return(false);
		    	}
		    });
		    $("#lgsubmit").click(function(){
				var lang = document.getElementById("lgbox").value;
				var feedback = document.getElementById("feedbackbox").value;
				if (lang.length > 2) {
				    //lang = lang.slice(3,lang.length);
				    experiment.data.language = lang;
				    experiment.data.feedback = feedback;
				    showSlide("finished");
				    setTimeout(function() { turk.submit(experiment.data) }, 1000);
				}
				return(false);
			});
		}
		else if (num < 3){
			$(".name1").html(name_1);
			$(".name2").html(name_2);
			showSlide("sentence_judgment");
			var qdata = {};
			var startTime = (new Date()).getTime(); 
			var subject = features[num]['subject'];
			var number = features[num]['number'];
			var definiteness = features[num]['definiteness'];
			var sentence = features[num]['sentence'];
			var context = features[num]['context'];
			var tense = features[num]['tense'];
			var qud = make_qud(subject,context);
			qdata.qud = qud;
			qdata.subject = subject;
			qdata.animacy = features[num]['animacy'];
			qdata.number = number;
			qdata.definiteness = definiteness;
			qdata.sentence = sentence;
			qdata.tense = tense;
			qdata.context = context;
			qdata.type = 'example'
			var genericity_response;
			var genericity_answered;
			var explanation = false;
			
			if (context == 'bare'){
				$('#context').hide()
			}
			else{
				$('#context').show()
			}
			
			$(".no_answer").hide();
			
			$("#explanation").hide();
			
			$(".progress").hide();
			
			$('#context-utterance').html(qud);
			
			$('#response-utterance').html(sentence);
			
			$("#subject").html(subject);
			
			$('.sentence').html(sentence);
						
			$(".plural_subject").html(pluralize(subject));
			
			$(".cap_plural_subject").html(capitalize(pluralize(subject)));
			
			if (qdata.number == 'singular'){
				$(".subject").html(subject);
			}
			else {
				$(".subject").html('group of ' + pluralize(subject));
			}
        	
        	$(".rating").change(function() {
				genericity_response = $(this).attr("value");
				genericity_answered = true;
        	});
			
			$(".continue").click(function() {
	    		var clickTime = (new Date()).getTime();
	    		if (!genericity_answered) { // test for answer meeting relevant parameters -- e.g., all questions answered
	    			// if no, show some text saying so and ask them to answer appropriately
	    			$(".no_answer").show();
	    		} 
	    		else if (genericity_answered && !explanation){
	    			var explanation_text = make_explanation(subject,parseInt(genericity_response));
	    			$(".explain_text").html(explanation_text);
	    			$(".no_answer").hide();
	    			$("#explanation").show();
	    			explanation = true;
	    		}
	    		else { // advance to next question
	    			var endTime = (new Date()).getTime(); 
	    			qdata.genericity = genericity_response;
	    			qdata.rt = endTime - startTime;
	    			$(".continue").unbind('click'); // remove this fxn after using it once, so we can reuse the button
	    			$('.rating').attr('checked',false);
	    			experiment.data['q' + num + 'data'] = qdata; // add trial data to experiment.data object, which is what we'll eventually submit to MTurk
	    			experiment.next(num + 1);
	    		}
	    	});
		} 
		else if (num == 3){
			showSlide("end_of_examples");
			$(".continue").click(function() {
	    		experiment.next(num + 1);
	    	});
		} 
 		else {
 			$(".name1").html(name_1);
			$(".name2").html(name_2);
			showSlide("sentence_judgment");
			var qdata = {};
			var startTime = (new Date()).getTime(); 
			var subject = features[num-1]['subject'];
			var number = features[num-1]['number'];
			var definiteness = features[num-1]['definiteness'];
			var context = features[num-1]['context'];
			var tense = features[num-1]['tense'];
			var sentence = make_response(subject,number,definiteness,tense);
			var qud = make_qud(subject,context);
			qdata.qud = qud;
			qdata.subject = subject;
			qdata.animacy = features[num-1]['animacy'];
			qdata.number = number;
			qdata.definiteness = definiteness;
			qdata.sentence = sentence;
			qdata.tense = tense;
			qdata.context = context;
			qdata.type = 'target'
			var genericity_response;
			var genericity_answered;
			var example_shown = false;
			
			if (context == 'bare'){
				$('#context').hide()
			}
			else {
				$('#context').show()
			}
			
			$(".no_answer").hide();
			
			$("#explanation").hide();
			
			$(".progress").hide();
			
			$('#context-utterance').html(qud);
			
			$('#response-utterance').html(sentence);
			
			$("#subject").html(subject);
			
			$('.sentence').html(sentence);
						
			$(".plural_subject").html(pluralize(subject));
			
			$(".cap_plural_subject").html(capitalize(pluralize(subject)));
			
			if (qdata.number == 'singular'){
				$(".subject").html(subject);
			}
			else {
				$(".subject").html('group of ' + pluralize(subject));
			}
		    
		    $(".rating").change(function() {
				genericity_response = $(this).attr("value");
				genericity_answered = true;
        	});
			
			$(".continue").click(function() {
	    		var clickTime = (new Date()).getTime();
	    		if (!genericity_answered) { // test for answer meeting relevant parameters -- e.g., all questions answered
	    			// if no, show some text saying so and ask them to answer appropriately
	    			$(".no_answer").show();
	    		}
	    		else { // advance to next question
	    			var endTime = (new Date()).getTime(); 
	    			qdata.genericity = genericity_response;
	    			qdata.rt = endTime - startTime;
	    			$(".continue").unbind('click'); // remove this fxn after using it once, so we can reuse the button
	    			$('.rating').attr('checked',false);
	    			experiment.data['q' + num + 'data'] = qdata; // add trial data to experiment.data object, which is what we'll eventually submit to MTurk
	    			experiment.next(num + 1);
	    		}
	    	});
	}
	}
}