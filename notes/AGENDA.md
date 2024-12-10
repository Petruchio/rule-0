# Features:
* Get dependency tree for views and materialized views.
* Rebase a view.
* Destroy and re-create a view tree.
* Refresh a materialized view tree in dependency order.
* View which gives information on partitions.
* Create an updatable view for the user's saved search path.

# Fixes:
	Modify find_in_path() to take either a single argument (the object),
	or two arguments (the schema and the object).  While it seems
	pointless to look for something if you already know where it is,
	this would allow the function to be used to find out if a particular
	thing exists in a particular place.  A null result means, "no".

# Concerns:

Much as I love clean names, grabbing prime real estate like "meta" and
"update" for Rule 0 schemata isn't reasonable.  Using a prefix like r0_
would solve this, and correspond well to Postgres' pg_ prefix, but that's
a tad annoying for tab completion.  A suffix would just be weird, though,
and gives tab completion priority to Rule 0 stuff over the actual contents
of the database.  I'll just have to bite the bullet and make a decision soon.

The meta schema mostly holds system information.  The search_path view doesn't
really fit with the rest; it pertains specifically to the current user.  There
should be a schema for that kind of thing.

# Gratuitious Complaints:

PostgreSQL *really* needs nested schemata.  Having a single level of namespaces
is just nonsense.  Imagine if your filesystem supported only top-level
directories!  I think that was a thing in UNIX in the early '70s.

Tom Lane was actually interested in the idea, at one point, but gave up on it
because of difficulties with SQL's syntax.  I think it needs to be revisited.
