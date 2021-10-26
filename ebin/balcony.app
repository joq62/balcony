%% This is the application resource file (.app file) for the 'base'
%% application.
{application, balcony,
[{description, "balcony controller" },
{vsn, "1.0.0" },
{modules, 
	  [balcony_app,balcony_sup,balcony,balcony_server]},
{registered,[balcony]},
{applications, [kernel,stdlib]},
{mod, {balcony_app,[]}},
{start_phases, []}
]}.
