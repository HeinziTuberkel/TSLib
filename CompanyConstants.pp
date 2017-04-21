unit CompanyConstants;

{
  Diese Unit enthält die Firmenkonstanten, die von TSApp verwendet werden.
  Damit werden z.B. Basiverzeichnis für die Globale INI und Mailadresse für
  Fehlerberichte voreingestellt.

  Wird diese .pas-Datei in den Standard-Suchpfad für Anwendungen abgelegt,
  so wird sie in die Anwendung hineinkompiliert und damit werden die Voreinstellungen
  für alle Anwendungen festgelegt.

  Alternativ kann die Unit im Verzeichnis der TailorWare-Packages abgelegt werden,
  dann müssen für Änderungen des Firmennamens die Packages neu kompiliert werden.
}

interface

const
  DefaultCompanyName = 'Tailor Soft';
  DefaultCompanyMailAddress = 'support@tailorsoft.de';

  DefaultComponentPage = 'Tailor Ware';
  DefaultComponentPageDB = DefaultComponentPage + ' Database';


implementation

end.

