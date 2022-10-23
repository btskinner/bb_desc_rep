// -----------------------------------------------------------------------------
//
// [ PROJ ] Variation in broadband access among undergraduate populations
//          across the United States
// [ FILE ] mrp.stan
// [ AUTH ] Benjamin Skinner (@btskinner), Taylor Burtch, & Hazel Levy
// [ INIT ] 22 October 2022
//
// -----------------------------------------------------------------------------
functions {
  // function to perform multithreaded loglik summation
  real partial_sum_lpmf(array[] int y_slice,
			int start, int end,
			array[] int tot,
			vector f,
			real a,
			real b_f,
			vector a_re,
			vector a_re_f,
			vector a_st_re,
			vector a_st_re_f,
			vector a_finc,
			vector a_age,
			vector a_state,
			vector a_region,
			array[] int re,
			array[] int stre,
			array[] int finc,
			array[] int age,
			array[] int state,
			array[] int region) {
    // binomial likelihood model split according to grainsize
    return binomial_logit_lupmf(y_slice | tot[start:end],
				a
				+ f[start:end] * b_f
				+ a_re[re[start:end]]
				+ f[start:end] .* a_re_f[re[start:end]]
				+ a_finc[finc[start:end]]
				+ a_age[age[start:end]]
				+ a_st_re[stre[start:end]]
				+ f[start:end] .* a_st_re_f[stre[start:end]]
				+ a_state[state[start:end]]
				+ a_region[region[start:end]]);
  }
}
data {
  int<lower=1> R;	 // # regions
  int<lower=1> S;	 // # states
  int<lower=1> N;	 // # obs
  int<lower=1> J_re;	 // # race/ethnic categories
  int<lower=1> J_finc;   // # financial categories
  int<lower=1> J_age;    // # age groups
  int<lower=1> J_stre;   // # state X race/ethnic groups
  array[N] int y;        // outcome (binomial successes, n)
  array[N] int tot;      // total (binomial total, N)
  vector[N] f;           // == 1 female
  array[N] int re;       // race/ethnic categories
  array[N] int stre;     // race/ethnic X state categories
  array[N] int finc;     // financial categories
  array[N] int age;      // age categories
  array[N] int region;   // regions
  array[N] int state;    // states
}
parameters {
  // single parameters: overall and beta_female
  real a;
  real b_f;

  // non-centered random intercepts: region and state
  real<lower=0> s_a_region;
  vector<multiplier=s_a_region>[R] a_region;
  real<lower=0> s_a_state;
  vector<multiplier=s_a_state>[S] a_state;

  // non-centered random intercepts: race/ethnicity, financial, and age
  real<lower=0> s_a_re;
  vector<multiplier=s_a_re>[J_re] a_re;
  real<lower=0> s_a_finc;
  vector<multiplier=s_a_finc>[J_finc] a_finc;
  real<lower=0> s_a_age;
  vector<multiplier=s_a_age>[J_age] a_age;
  
  // non-centered random intercepts: interactions
  real<lower=0> s_a_re_f;
  vector<multiplier=s_a_re_f>[J_re] a_re_f;
  real<lower=0> s_a_st_re;
  vector<multiplier=s_a_st_re>[J_stre] a_st_re;
  real<lower=0> s_a_st_re_f;
  vector<multiplier=s_a_st_re_f>[J_stre] a_st_re_f;
}
model {
  // priors: coefficients		       
  a ~ normal(0,1);
  b_f ~ normal(0,1);
  a_finc ~ normal(0,s_a_finc);
  a_age ~ normal(0,s_a_age);
  a_state ~ normal(0,s_a_state);
  a_region ~ normal(0,s_a_region);
  a_re ~ normal(0,s_a_re);
  a_re_f ~ normal(0,s_a_re_f);
  a_st_re ~ normal(0,s_a_st_re);
  a_st_re_f ~ normal(0,s_a_st_re_f);

  // priors: standard deviations
  s_a_finc ~ normal(0,1);
  s_a_age ~ normal(0,1);
  s_a_state ~ normal(0,1);
  s_a_region ~ normal(0,1);
  s_a_re ~ normal(0,1);
  s_a_re_f ~ normal(0,1);
  s_a_st_re ~ normal(0,1);
  s_a_st_re_f ~ normal(0,1);

  // grainsize estimated automatically with = 1
  int grainsize = 1;
  // likelihood using reduce_sum()
  // NB: arguments in same order as above
  target += reduce_sum(partial_sum_lupmf, y,
		       grainsize,
		       tot,
                       f,
		       a,
		       b_f,
		       a_re,
		       a_re_f,
		       a_st_re,
		       a_st_re_f,
		       a_finc,
		       a_age,
		       a_state,
		       a_region,
		       re,
		       stre,
		       finc,
		       age,
		       state,
		       region
		       );
}
