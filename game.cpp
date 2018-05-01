#include <iostream>
#include <string>
#include <vector>
#include <SWI-Prolog.h>

#pragma GCC diagnostic ignored "-Wwrite-strings"

using namespace std;

int PL_get_string(term_t t, string &s) {
	int i = 0;
	size_t len = 0;
	char *c_str;
	
	i = PL_get_string_chars(t,&c_str,&len);
	string str(c_str);
	s = str;

	return 1;
}

void readInChoice(vector<string> &v, term_t t) {
	// nested list
	string temp;
	
	term_t h1 = PL_new_term_ref();
	term_t h2 = PL_new_term_ref();

	while(PL_get_list(t, h1, t)) {
		while(PL_get_list(h1, h2, h1)) {
			int i = PL_get_string(h2,temp);
			v.push_back(temp);
		}
	}
	return;
}

int main() {
	putenv("SWI_HOME_DIR=/usr/lib/swi-prolog");
	//_putenv("SWI_HOME_DIR=C:\\dev\\swipl");

	char flag[] = "-l";
	char file[] = "game.pl";
	static char *av[] = { flag, file, NULL };
	
	if(!PL_initialise(2,av)) {
		cout << "error initializing" << endl;
		PL_halt(1);
	} 
	
	predicate_t sample_gs = PL_predicate("sample_gs_atom",1,"user");
	term_t gsTerm = PL_new_term_refs(1);
	int q = PL_open_query(NULL, PL_Q_EXT_STATUS, sample_gs, gsTerm);

	int i = 0;
	char *atom = NULL;
	
	if(PL_next_solution(q) > 0) {
		i = PL_get_atom_chars(gsTerm, &atom);
	}

	PL_close_query(q);

	vector<vector<string> > choices = vector<vector<string> >();
	vector<char*> states = vector<char*>();
	states.push_back(atom);
	
	predicate_t step_n = PL_predicate("step_n_atom",4,"user");

	int selection = 0;

	do {
		term_t steps = PL_new_term_refs(4);
		term_t n = steps;
		term_t inGS = steps+1;
		term_t outGS = steps+2;
		term_t actions = steps+3;

		i = PL_put_integer(n,1);
		i = PL_put_atom_chars(inGS,states[selection]);
		
		//if(states.size() > 1) {
			cout << states[selection] << endl << endl;
		//}

		int q = PL_open_query(NULL, PL_Q_EXT_STATUS, step_n, steps);
		
		states.clear();
		choices.clear();

		while(PL_next_solution(q) > 0) {
			i = PL_get_atom_chars(outGS, &atom);
			states.push_back(atom);

			vector<string> choice = vector<string>();
			readInChoice(choice,actions);
			choices.push_back(choice);
		}

		PL_close_query(q);
	
		// Uncomment the following for autosteps
		if(choices.size() == 1) {
			cout << choices[0][0] << endl;
			selection = 0;
			continue;
		}

		cout << endl;
		for(int i = 0; i < choices.size(); i++) {
			cout << i << ". ";
			for(int j = 0; j < choices[i].size(); j++) {
				cout << choices[i][j] << " ";
			}
			cout << endl;
		}

		if(choices.size() == 0) {
			break;
		}

		cout << choices.size() << ". Exit." << endl;
		cout << "Selection: ";
		cin >> selection;
	} while(selection < choices.size());
	
	cout << "No more choices, exiting." << endl;
	PL_halt(1);
 	return 0;
}