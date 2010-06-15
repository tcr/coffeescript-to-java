// Mug
window.Mug = {};

Mug.compile = function () {
	// reset globals
	Mug.scopes = [];
	Mug.classes = [];
	Mug.numbers = {};
	Mug.props = {};

    var coffee = CoffeeScript.compile(document.getElementById('input').value, {
        no_wrap: true
    });
    scopes = "";
    objs = "";

    //
    // compiled code
    //
    var data = "";
    for (var i = 0; i < Mug.scopes.length; i++) {
        var scope = Mug.scopes[i];
        data += "final CSScope" + scope.id + " s" + scope.id + " = new CSScope" + scope.id + "();\n";
    }

    for (var num in Mug.numbers)
    data += "final CSNumber _" + num + " = new CSNumber(" + num + ");\n";
    data += "\n\n" + coffee;
    data += "\n\t}";
    data = "\tpublic static void run() throws Exception {\n" + data;

    //
    // scopes
    //
    delete Mug.scopes[0].variables.print
    delete Mug.scopes[0].variables.nanoTime

    for (var i = 0; i < Mug.scopes.length; i++) {
        var s = Mug.scopes[i];
        scopes += "\tstatic class CSScope" + s.id + (s.id == 0 ? " extends CSScopeRoot" : "") + " {\n\t\tpublic void clean() { ";
        var a = "";
        for (var name in s.variables) {
            if (s.variables[name] == "param") continue;
            a += "_" + name + " = ";
        }
        if (a.length) scopes += a + "null; ";
        scopes += "}\n";
        for (var name in s.variables) {
            if (s.variables[name] == "param") continue;
            scopes += "\n\t\tCSPrimitive _" + name + ";\n" + "\t\tpublic CSPrimitive get_" + name + "() { return _" + name + "; }\n" + "\t\tpublic void set_" + name + "(CSPrimitive v) { _" + name + " = v; }\n";
        }
        scopes += "\t}\n\n";
    }

    //
    // objects
    //
    objs = "\tstatic public class CSObject extends CSObjectBase {\n\t\tpublic CSObject() { }\n\t\tpublic CSObject(CSObject proto) { __proto__ = proto; }\n";
    for (var name in Mug.props) {
        objs += "\n\t\tpublic CSPrimitive get_" + name + "() { return get(\"" + name + "\"); }\n";
        objs += "\t\tpublic void set_" + name + "(CSPrimitive v) { set(\"" + name + "\", v); }\n";
    }
    objs += "\t}\n\n";

    for (var i = 0; i < Mug.classes.length; i++) {
        var c = Mug.classes[i];
        objs += "\tstatic public class CSObject_" + c.id + " extends CSObject {\n\t\tpublic CSObject_" + c.id + "() { }\n\t\tpublic CSObject_" + c.id + "(CSObject proto) { __proto__ = proto; }\n";
        for (var j = 0; j < c.properties.length; j++) {
            if (c.properties[j].variable.last) {
                var name = c.properties[j].variable.last;
                objs += "\n\t\tCSPrimitive _" + name + ";\n\t\tpublic CSPrimitive get_" + name + "() {\n" + "\t\t\tif (_" + name + " != null) return _" + name + ";\n" + "\t\t\tif (__proto__ == null) return null;\n" + "\t\t\treturn __proto__.get_" + name + "();\n" + "\t\t}\n" + "\t\tpublic void set_" + name + "(CSPrimitive v) { _" + name + " = v; }\n";
            }
        }
        objs += "\t}\n\n";
    }

    // done
    document.getElementById('data').value = insert("\t/*\n\t * objects\n\t */\n\n" + objs + "\n\n" + "\t/*\n\t * scopes\n\t */\n\n" + scopes + "\n\n" + "\t/*\n\t * code\n\t */\n\n" + data);

    function insert(gen) {
        return "package mug;\n\nimport java.util.HashMap;\n\npublic class Script {\n\tpublic static void main(String[] arg) {\n\t\ttry {\n\t\t\tSystem.out.println(\"[SCRIPT START]\");\n\t\t\tlong start = System.nanoTime();\n\t\t\trun();\n\t\t\tSystem.out.println(\"[SCRIPT END]\\nTime: \" + (System.nanoTime() - start) / 1000000000f + \" seconds.\");\n\t\t} catch (Exception e) {\n\t\t\te.printStackTrace();\n\t\t}\n\t}\n\n\t/*\n\t * interfaces\n\t */\n\n\tstatic class CSPrimitive {\n\t}\n\n\t/*\n\t * primitives\n\t */\n\n\tstatic class CSBoolean extends CSPrimitive {\n\t\tboolean value = false;\n\n\t\tpublic CSBoolean(boolean value) {\n\t\t\tthis.value = value;\n\t\t}\n\n\t\t// statics\n\t\tstatic public CSBoolean TRUE = new CSBoolean(true);\n\t\tstatic public CSBoolean FALSE = new CSBoolean(false);\n\t}\n\n\tstatic class CSNumber extends CSPrimitive {\n\t\tdouble value = 0;\n\n\t\tpublic CSNumber(double value) {\n\t\t\tthis.value = value;\n\t\t}\n\t}\n\n\tstatic class CSString extends CSPrimitive {\n\t\tString value = \"\";\n\n\t\tpublic CSString(String value) {\n\t\t\tthis.value = value;\n\t\t}\n\t}\n\n\t/*\n\t * objects\n\t */\n\n\tabstract static public class CSObjectBase extends CSPrimitive {\n\t\tCSObject __proto__;\n\n\t\tpublic CSObject getProto() {\n\t\t\treturn __proto__;\n\t\t}\n\n\t\tHashMap<String, CSPrimitive> hash;\n\n\t\tpublic CSPrimitive get(String key) {\n\t\t\tCSPrimitive ret;\n\t\t\tif (hash == null || ((ret = hash.get(key)) == null \&\& __proto__ != null))\n\t\t\t\treturn __proto__.get(key);\n\t\t\treturn ret;\n\t\t}\n\n\t\tpublic void set(String key, CSPrimitive value) {\n\t\t\tif (hash == null)\n\t\t\t\thash = new HashMap<String, CSPrimitive>();\n\t\t\thash.put(key, value);\n\t\t}\n\n\t\tCSPrimitive _prototype;\n\n\t\tpublic CSPrimitive get_prototype() {\n\t\t\tif (_prototype != null)\n\t\t\t\treturn _prototype;\n\t\t\tif (__proto__ == null)\n\t\t\t\treturn null;\n\t\t\treturn __proto__.get_prototype();\n\t\t}\n\n\t\tpublic void set_prototype(CSPrimitive v) {\n\t\t\t_prototype = v;\n\t\t}\n\t}\n\n\tstatic abstract class CSFunction extends CSObject {\n\t\tpublic CSFunction() {\n\t\t\tset_prototype(createProto());\n\t\t}\n\n\t\tCSObject actual_prototype;\n\n\t\tpublic void set_prototype(CSPrimitive v) {\n\t\t\t_prototype = v;\n\t\t\tactual_prototype = (v instanceof CSObject ? (CSObject) v : new CSObject());\n\t\t}\n\n\t\t// overrides\n\t\tpublic CSObject createProto() {\n\t\t\treturn new CSObject();\n\t\t}\n\n\t\tpublic CSObject createNew() {\n\t\t\treturn new CSObject(actual_prototype);\n\t\t}\n\n\t\tpublic CSPrimitive instantiate(CSPrimitive l0, CSPrimitive l1, CSPrimitive l2, CSPrimitive l3) throws Exception {\n\t\t\tCSObject obj = createNew();\n\t\t\tCSPrimitive ret;\n\t\t\tif ((ret = invoke(obj, l0, l1, l2, l3)) != null)\n\t\t\t\treturn ret;\n\t\t\treturn obj;\n\t\t}\n\n\t\tpublic abstract CSPrimitive invoke(CSObject ths, CSPrimitive l0, CSPrimitive l1, CSPrimitive l2, CSPrimitive l3)\n\t\t\t\tthrows Exception;\n\t}\n\n\t/*\n\t * conversions\n\t */\n\n\tstatic boolean asBoolean(CSPrimitive a) {\n\t\tif (a instanceof CSBoolean)\n\t\t\treturn ((CSBoolean) a).value;\n\t\t// \n\t\t// \n\t\t// \n\t\treturn false;\n\t}\n\n\tstatic double asNumber(CSPrimitive a) {\n\t\tif (a instanceof CSNumber)\n\t\t\treturn ((CSNumber) a).value;\n\t\tif (a instanceof CSBoolean)\n\t\t\treturn ((CSBoolean) a).value ? 1 : 0;\n\t\t// \n\t\t// \n\t\treturn Double.NaN;\n\t}\n\n\tstatic String asString(CSPrimitive a) {\n\t\tif (a instanceof CSString)\n\t\t\treturn ((CSString) a).value;\n\t\tif (a instanceof CSNumber)\n\t\t\treturn Double.toString(((CSNumber) a).value);\n\t\tif (a instanceof CSBoolean)\n\t\t\treturn Boolean.toString(((CSBoolean) a).value);\n\t\t// \n\t\treturn null;\n\t}\n\n\t/*\n\t * root scope\n\t */\n\n\tstatic class CSScopeRoot {/*\n\t\tpublic CSScopeRoot() {\n\t\t\t_Math.set_max(new CSFunction() {\n\t\t\t\tpublic CSPrimitive invoke(final CSObject ths, CSPrimitive a, CSPrimitive b, CSPrimitive l2, CSPrimitive l3)\n\t\t\t\t\tthrows Exception {\n\t\t\t\t\treturn asNumber(a) > asNumber(b) ? a : b;\n\t\t\t\t}\n\t\t\t});\n\t\t}*/\n\t\t\n\t\tCSFunction _print = new CSFunction() {\n\t\t\tpublic CSPrimitive invoke(final CSObject ths, CSPrimitive value, CSPrimitive l1, CSPrimitive l2, CSPrimitive l3)\n\t\t\t\t\tthrows Exception {\n\t\t\t\tSystem.out.println(asString(value));\n\t\t\t\treturn null;\n\t\t\t}\n\t\t};\n\t\tpublic CSPrimitive get_print() { return _print; }\n\n\t\tCSFunction _nanoTime = new CSFunction() {\n\t\t\tpublic CSPrimitive invoke(final CSObject ths, CSPrimitive value, CSPrimitive l1, CSPrimitive l2, CSPrimitive l3)\n\t\t\t\t\tthrows Exception {\n\t\t\t\treturn new CSNumber(System.nanoTime());\n\t\t\t}\n\t\t};\n\t\tpublic CSPrimitive get_nanoTime() { return _nanoTime; }\n\t\t/*\n\t\tCSObject _Math = new CSObject();\n\t\tpublic CSObject get_Math() { return _Math; }*/\n\t}\n\n\t// ##################################################################################################\n\t// ##################################################################################################\n\t// GENERATED CODE\n\t// ##################################################################################################\n\t// ##################################################################################################\n\n" + gen + "\n}";
    }

    // reset coffeescript things
    ClassNode.id = 0;
    Scope.id = 0;
};