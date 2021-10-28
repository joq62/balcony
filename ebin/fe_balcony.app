%% This is the application resource file (.app file) for the 'base'
%% application.
{application, fe_balcony,
[{description, "fe_balcony controller" },
{vsn, "1.0.0" },
{modules, 
	  [fe_balcony_app,fe_balcony_sup,fe_balcony,fe_balcony_server]},
{registered,[fe_balcony]},
{applications, [kernel,stdlib]},
{mod, {fe_balcony_app,[]}},
{start_phases, []}
]}.
